"""Cache USGS upstream basins after same-stream validation; Python standard library only.

Run from North_Puget_Sound after prepare_phase1_basin_requests() in R.
No habitat linework is added. Failed/unvalidated requests remain explicit audit rows.
"""
import argparse
import concurrent.futures
import datetime
import hashlib
import json
import math
from pathlib import Path
import re
import subprocess
import time
import urllib.parse


def cached_source_path(value):
    """Resolve historical cache paths within the current regional checkout."""
    normalized = str(value).replace("\\", "/")
    marker = "/spatial_data/"
    if normalized.startswith("spatial_data/"):
        suffix = normalized
    elif marker in normalized:
        suffix = "spatial_data/" + normalized.rsplit(marker, 1)[1]
    else:
        return Path(normalized)
    return Path(__file__).resolve().parents[1] / suffix


def name_key(value):
    text = re.sub(r"[^a-z0-9]+", " ", str(value or "").lower()).strip()
    if not text or re.match(r"^(unnamed|unknown|unidentified|not named)( |$)", text):
        return None
    for pattern, replacement in [(r"\b(sf|s f|s fork|southfork)\b", "south fork"),
                                 (r"\b(nf|n f|n fork|northfork)\b", "north fork"),
                                 (r"\b(mf|m f|m fork|middlefork)\b", "middle fork"),
                                 (r"\b(r|riv)\b", "river"), (r"\b(cr|ck)\b", "creek"),
                                 (r"\bstilly\b", "stillaguamish")]:
        text = re.sub(pattern, replacement, text)
    text = re.sub(r"^(.+) (south|north|middle) fork$", r"\2 fork \1", text)
    return re.sub(r" +", " ", text)


def names_match(a, b):
    a, b = name_key(a), name_key(b)
    if a is None or b is None:
        return False
    if a == b:
        return True
    suffix_a, suffix_b = re.search(r" (river|creek)$", a), re.search(r" (river|creek)$", b)
    return not (suffix_a and suffix_b) and re.sub(r" (river|creek)$", "", a) == re.sub(r" (river|creek)$", "", b)


def local_xy(coordinate, origin):
    # WGS84 local metric approximation, shared by both supports; R revalidates
    # source/outlet distances in the project's projected CRS on load.
    phi = math.radians(origin[1])
    e2, a = 0.0066943799901413165, 6378137.0
    denominator = 1 - e2 * math.sin(phi) ** 2
    meridian = a * (1 - e2) / denominator ** 1.5
    prime = a / math.sqrt(denominator)
    return ((coordinate[0] - origin[0]) * math.pi / 180 * prime * math.cos(phi),
            (coordinate[1] - origin[1]) * math.pi / 180 * meridian)


def _line_projection(point, line):
    best = None
    cumulative = 0.0
    for first, last in zip(line[:-1], line[1:]):
        delta = last[0] - first[0], last[1] - first[1]
        squared = delta[0] ** 2 + delta[1] ** 2
        length = math.sqrt(squared)
        if squared:
            fraction = max(0, min(1, ((point[0] - first[0]) * delta[0] +
                                      (point[1] - first[1]) * delta[1]) / squared))
            distance = math.hypot(point[0] - first[0] - fraction * delta[0],
                                  point[1] - first[1] - fraction * delta[1])
            if best is None or distance < best[0]:
                best = distance, cumulative + fraction * length
        cumulative += length
    return best, cumulative


def _line_at(line, measure):
    remaining = max(0.0, measure)
    for first, last in zip(line[:-1], line[1:]):
        length = math.hypot(last[0] - first[0], last[1] - first[1])
        if length and remaining <= length:
            fraction = remaining / length
            return (first[0] + fraction * (last[0] - first[0]),
                    first[1] + fraction * (last[1] - first[1]))
        remaining -= length
    return tuple(line[-1])


def route_support(request, feature):
    geometry = feature["geometry"]
    lines = geometry["coordinates"] if geometry["type"] == "MultiLineString" else [geometry["coordinates"]]
    origin = [request["longitude"], request["latitude"]]
    metric_lines = [[local_xy(point, origin) for point in line] for line in lines]
    halfspan = request.get("course_tangent_halfspan_m", 25)
    distances, angles = [], []
    for coordinate, heading in zip(request["support_samples"], request["support_headings"]):
        point = local_xy(coordinate, origin)
        start, end = local_xy(heading[0], origin), local_xy(heading[1], origin)
        own = end[0] - start[0], end[1] - start[1]
        candidates = []
        for line in metric_lines:
            hit, length = _line_projection(point, line)
            if hit is not None:
                candidates.append((hit[0], hit[1], length, line))
        if not candidates:
            distances.append(float("inf"))
            angles.append(90)
            continue
        distance, measure, length, line = min(candidates, key=lambda value: value[0])
        before = _line_at(line, max(0, measure - halfspan))
        after = _line_at(line, min(length, measure + halfspan))
        reference = after[0] - before[0], after[1] - before[1]
        norm = math.hypot(*own) * math.hypot(*reference)
        cosine = abs(own[0] * reference[0] + own[1] * reference[1]) / norm if norm else 0
        angle = math.degrees(math.acos(min(1, max(0, cosine)))) if norm else 90
        distances.append(distance)
        angles.append(angle)
    coverage = sum(distance <= request["max_match_distance_m"] and angle <= request["max_heading_degrees"]
                   for distance, angle in zip(distances, angles)) / len(distances)
    return dict(route_support_coverage=coverage, route_max_distance_m=max(distances),
                route_max_heading_degrees=max(angles), course_tangent_halfspan_m=halfspan)


def project_route(request, feature):
    geometry = feature["geometry"]
    lines = geometry["coordinates"] if geometry["type"] == "MultiLineString" else [geometry["coordinates"]]
    origin = [request["longitude"], request["latitude"]]
    best = None
    for line in lines:
        for start, end in zip(line[:-1], line[1:]):
            first, last = local_xy(start, origin), local_xy(end, origin)
            delta = last[0] - first[0], last[1] - first[1]
            squared = delta[0] ** 2 + delta[1] ** 2
            if not squared:
                continue
            fraction = max(0, min(1, -(first[0] * delta[0] + first[1] * delta[1]) / squared))
            distance = math.hypot(first[0] + fraction * delta[0], first[1] + fraction * delta[1])
            if best is None or distance < best[0]:
                best = distance, [start[0] + fraction * (end[0] - start[0]),
                                  start[1] + fraction * (end[1] - start[1])]
    if best is None:
        raise ValueError("No projectable reference segments")
    return best


def request_json(url, path, payload=None, refresh=False):
    if path.exists() and not refresh:
        return json.loads(path.read_text(encoding="utf-8"))
    command = ["curl.exe", "--ssl-no-revoke", "--fail", "--silent", "--show-error", "--location",
               "--connect-timeout", "20", "--max-time", "120", "--output", str(path.with_suffix(".part"))]
    if payload is not None:
        body = path.with_suffix(".request.json")
        body.write_text(json.dumps(payload), encoding="utf-8")
        command += ["--header", "Content-Type: application/json", "--data-binary", "@" + str(body)]
    command.append(url)
    last_error = None
    for attempt in range(3):
        result = subprocess.run(command, capture_output=True, text=True, timeout=135)
        if result.returncode == 0:
            parsed = json.loads(path.with_suffix(".part").read_text(encoding="utf-8"))
            path.with_suffix(".part").replace(path)
            return parsed
        last_error = result.stderr.strip()
        if attempt < 2:
            time.sleep(2 * (attempt + 1))
    raise RuntimeError(last_error)


def process(request, cache, refresh=False, recover_index=False):
    signature = hashlib.sha256(json.dumps(request, sort_keys=True).encode()).hexdigest()
    folder = cache / (hashlib.sha256(request["review_record_key"].encode()).hexdigest()[:12] + "_" + signature[:12])
    folder.mkdir(parents=True, exist_ok=True)
    manifest_path = folder / "manifest.json"
    if manifest_path.exists() and not refresh:
        cached = json.loads(manifest_path.read_text(encoding="utf-8"))
        if not (recover_index and not cached.get("index_recovery_attempted")
                and cached.get("status") not in ["basin_delineated_pending_projected_validation", "special_gorge_loader"]):
            return cached
        (folder / "manifest.hydrolocation_history.json").write_text(json.dumps(cached, indent=2), encoding="utf-8")
    (folder / "request.json").write_text(json.dumps(request, indent=2), encoding="utf-8")
    audit = {key: request.get(key) for key in ["review_record_key", "review_number", "SiteId", "StreamName", "graph_edge_id", "longitude", "latitude"]}
    audit.update(request_sha256=signature, cache_folder=str(folder), approved=False,
                 status="not_attempted", retrieved_utc=datetime.datetime.now(datetime.timezone.utc).isoformat())
    audit["index_recovery_attempted"] = recover_index
    audit["indexing_method"] = "validated_route_projection_after_catchment_lookup" if recover_index else "USGS_hydrolocation"
    try:
        if request.get("preparation_status") != "ready":
            audit["status"] = request.get("preparation_status", "support_not_prepared")
            return audit
        base = "https://api.water.usgs.gov"
        url = base + ("/nldi/linked-data/comid/position?" if recover_index else "/nldi/linked-data/hydrolocation?") + urllib.parse.urlencode(
            {"coords": f"POINT({request['longitude']} {request['latitude']})", "f": "json"})
        audit["request_stage"] = "containing_catchment_lookup" if recover_index else "hydrolocation"
        indexed = request_json(url, folder / ("catchment_position.json" if recover_index else "hydrolocation.json"), refresh=refresh)
        indexed = [item for item in indexed.get("features", []) if recover_index or item.get("properties", {}).get("source") == "indexed"]
        if len(indexed) != 1:
            raise ValueError("No unique indexed hydrolocation")
        indexed = indexed[0]
        audit["comid"] = str(indexed["properties"]["comid"])
        if not recover_index:
            coordinates = indexed["geometry"]["coordinates"]
            audit.update(indexed_longitude=coordinates[0], indexed_latitude=coordinates[1],
                         indexed_shift_m=math.hypot(*local_xy(coordinates, [request["longitude"], request["latitude"]])))
            if audit["indexed_shift_m"] > request["max_index_distance_m"]:
                audit["status"] = "indexed_point_beyond_tolerance"
                return audit
        url = base + "/fabric/pygeoapi/collections/nhdflowline_network/items?" + urllib.parse.urlencode({"comid": audit["comid"], "f": "json"})
        audit["request_stage"] = "named_reference_flowline"
        routes = request_json(url, folder / "named_flowline.json", refresh=refresh)
        routes = [feature for feature in routes.get("features", []) if str(feature.get("properties", {}).get("comid")) == audit["comid"]]
        if not routes and recover_index and (folder / "named_flowline.json").exists():
            previous = (folder / "named_flowline.json").read_text(encoding="utf-8")
            (folder / "named_flowline.hydrolocation_history.json").write_text(previous, encoding="utf-8")
            fresh = request_json(url, folder / "named_flowline.json", refresh=True)
            routes = [feature for feature in fresh.get("features", []) if str(feature.get("properties", {}).get("comid")) == audit["comid"]]
        if len(routes) != 1:
            raise ValueError("No unique matching COMID reference flowline")
        route = routes[0]
        audit["reference_stream_name"] = route["properties"].get("gnis_name")
        audit["reference_flowdir"] = route["properties"].get("flowdir")
        if not names_match(request["StreamName"], audit["reference_stream_name"]):
            audit["status"] = "reference_stream_name_unconfirmed"
            return audit
        audit.update(route_support(request, route))
        if audit["route_support_coverage"] < request["min_coverage"]:
            audit["status"] = "reference_local_course_unconfirmed"
            return audit
        if recover_index:
            distance, coordinates = project_route(request, route)
            audit.update(indexed_longitude=coordinates[0], indexed_latitude=coordinates[1], indexed_shift_m=distance)
            if distance > request["max_index_distance_m"]:
                audit["status"] = "validated_route_projection_beyond_tolerance"
                return audit
            verify_url = base + "/nldi/linked-data/comid/position?" + urllib.parse.urlencode(
                {"coords": f"POINT({coordinates[0]} {coordinates[1]})", "f": "json"})
            audit["request_stage"] = "projected_comid_confirmation"
            verified = request_json(verify_url, folder / "projected_position.json", refresh=refresh)
            verified_ids = {str(item.get("properties", {}).get("comid")) for item in verified.get("features", [])}
            if verified_ids != {audit["comid"]}:
                audit["status"] = "projected_comid_mismatch"
                return audit
        payload = {"inputs": [{"id": key, "type": "text/plain", "value": value} for key, value in
                              [("lat", str(coordinates[1])), ("lon", str(coordinates[0])), ("upstream", "true"), ("simplified", "false")]]}
        url = base + "/nldi/pygeoapi/processes/nldi-splitcatchment/execution?f=json"
        audit["request_stage"] = "split_catchment_execution"
        basin = request_json(url, folder / "split_basin.json", payload=payload, refresh=refresh)
        audit["basin_path"] = str(folder / "split_basin.json")
        audit["basin_md5"] = hashlib.md5((folder / "split_basin.json").read_bytes()).hexdigest()
        audit["status"] = "basin_delineated_pending_projected_validation"
        audit["approved"] = True
    except Exception as error:
        audit["status"] = "request_failed"
        audit["error"] = str(error)
    finally:
        manifest_path.write_text(json.dumps(audit, indent=2, allow_nan=False), encoding="utf-8")
    return audit


if __name__ == "__main__":
    import os as _workflow_os
    _workflow_os.chdir(Path(__file__).resolve().parents[1])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--requests", required=True)
    parser.add_argument("--cache-dir", default="spatial_data/upstream_basins/batch")
    parser.add_argument("--workers", type=int, choices=range(1, 5), default=3)
    parser.add_argument("--refresh", action="store_true")
    parser.add_argument("--limit", type=int)
    parser.add_argument("--recover-index-failures", action="store_true")
    parser.add_argument("--watch", action="store_true", help="Recover newly completed indexing failures until the primary batch finishes")
    parser.add_argument("--validated-position-default", action="store_true", default=True,
                        help="Default: named/course-validated COMID lookup and confirmed line projection")
    parser.add_argument("--hydrolocation-first", action="store_false", dest="validated_position_default",
                        help="Diagnostic alternative: use USGS hydrolocation indexing first")
    arguments = parser.parse_args()
    requests = json.loads(Path(arguments.requests).read_text(encoding="utf-8"))
    if arguments.limit is not None:
        requests = requests[:arguments.limit]
    cache = Path(arguments.cache_dir)
    cache.mkdir(parents=True, exist_ok=True)
    if arguments.recover_index_failures:
        while True:
            attempted = 0
            for request in requests:
                signature = hashlib.sha256(json.dumps(request, sort_keys=True).encode()).hexdigest()
                folder = cache / (hashlib.sha256(request["review_record_key"].encode()).hexdigest()[:12] + "_" + signature[:12])
                path = folder / "manifest.json"
                if not path.exists():
                    continue
                record = json.loads(path.read_text(encoding="utf-8"))
                if record.get("status") != "request_failed" or record.get("index_recovery_attempted") or (folder / "hydrolocation.json").exists():
                    continue
                recovered = process(request, cache, recover_index=True)
                attempted += 1
                print(f"RECOVERY {recovered['review_number']}: {recovered['status']}", flush=True)
            primary_path = cache / "manifest.json"
            primary = json.loads(primary_path.read_text(encoding="utf-8")) if primary_path.exists() else []
            if not arguments.watch or len(primary) >= len(requests):
                break
            time.sleep(5)
        raise SystemExit(0)
    results = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=arguments.workers) as pool:
        futures = [pool.submit(process, request, cache, arguments.refresh, arguments.validated_position_default) for request in requests]
        for future in concurrent.futures.as_completed(futures):
            result = future.result()
            results.append(result)
            (cache / "manifest.json").write_text(json.dumps(results, indent=2), encoding="utf-8")
            print(f"{len(results)}/{len(requests)}: {result['review_number']} {result['status']}", flush=True)
