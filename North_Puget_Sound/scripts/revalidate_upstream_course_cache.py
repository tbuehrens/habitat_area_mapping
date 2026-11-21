"""Reassess a changed local course method without moving cached basin outlets.

Run after prepare_phase1_basin_requests() writes new requests. Old requests and
their immutable source responses are preserved under their original hash paths.
Only newly passing course matches without a polygon execute a new basin POST.
"""
import argparse
import concurrent.futures
import datetime
import hashlib
import importlib.util
import json
from pathlib import Path
import shutil

spec = importlib.util.spec_from_file_location("basin_fetch", Path(__file__).with_name("fetch_upstream_basins.py"))
fetch = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fetch)

RESPONSE_FILES = ("named_flowline.json", "catchment_position.json", "projected_position.json",
                  "hydrolocation.json", "split_basin.json", "split_basin.request.json")


def signature(request):
    return hashlib.sha256(json.dumps(request, sort_keys=True).encode()).hexdigest()


def migrate(request, previous, cache):
    old_folder = fetch.cached_source_path(previous["cache_folder"])
    latest_path = old_folder / "manifest.json"
    if latest_path.exists():
        latest = json.loads(latest_path.read_text(encoding="utf-8"))
        if latest.get("request_sha256") == previous.get("request_sha256"):
            previous = latest
    old_request = json.loads((old_folder / "request.json").read_text(encoding="utf-8"))
    for key in ("review_record_key", "StreamName", "graph_edge_id", "longitude", "latitude"):
        if old_request.get(key) != request.get(key):
            raise ValueError(f"Cannot reuse response with changed source location/identity: {key}")
    folder = cache / (hashlib.sha256(request["review_record_key"].encode()).hexdigest()[:12] + "_" + signature(request)[:12])
    folder.mkdir(parents=True, exist_ok=True)
    for name in RESPONSE_FILES:
        source, target = old_folder / name, folder / name
        if source.exists() and not target.exists():
            shutil.copy2(source, target)
    # This file establishes the exact immutable pourpoint used by an existing
    # response. Neither tangent sampling nor cache migration can move it.
    if (folder / "split_basin.json").exists():
        payload_file = folder / "split_basin.request.json"
        if not payload_file.exists():
            raise ValueError("An existing basin must retain its original precise pourpoint payload")
        old_payload = json.loads(payload_file.read_text(encoding="utf-8"))
        indexed = {entry["id"]: entry["value"] for entry in old_payload["inputs"]}
        if abs(float(indexed["lon"]) - previous["indexed_longitude"]) > 1e-10 or \
                abs(float(indexed["lat"]) - previous["indexed_latitude"]) > 1e-10:
            raise ValueError("Cached basin payload and recorded indexed outlet disagree")
        (folder / "immutable_outlet_history.json").write_text(json.dumps(dict(
            original_cache_folder=str(old_folder), latitude=previous["indexed_latitude"],
            longitude=previous["indexed_longitude"], basin_md5=previous["basin_md5"]), indent=2), encoding="utf-8")
    (folder / "request.json").write_text(json.dumps(request, indent=2), encoding="utf-8")
    # Preserve known basin-service failures rather than repeatedly POSTing them
    # during a local-method reassessment. Their successful source route remains
    # available to prove coverage by a different downstream barrier.
    if previous.get("status") == "request_failed" and previous.get("request_stage") == "split_catchment_execution":
        updated = dict(previous, request_sha256=signature(request), cache_folder=str(folder),
                       course_revalidated_utc=datetime.datetime.now(datetime.timezone.utc).isoformat())
        raw = json.loads((folder / "named_flowline.json").read_text(encoding="utf-8"))
        route = [feature for feature in raw["features"] if str(feature["properties"]["comid"]) == str(previous["comid"])][0]
        updated.update(fetch.route_support(request, route))
        (folder / "manifest.json").write_text(json.dumps(updated, indent=2), encoding="utf-8")
        return updated
    # Early successful records predate indexing_method; the R loader has always
    # interpreted those as the original USGS hydrolocation method.
    recover = (previous.get("indexing_method") or "USGS_hydrolocation") != "USGS_hydrolocation"
    # Reassess the method rather than trusting a prior local-method decision.
    # Immutable response files and original hash-folder manifests are retained.
    (folder / "manifest.json").unlink(missing_ok=True)
    result = fetch.process(request, cache, recover_index=recover)
    history = folder / "immutable_outlet_history.json"
    if history.exists():
        immutable = json.loads(history.read_text(encoding="utf-8"))
        if result.get("basin_md5") != immutable["basin_md5"] or \
                abs(result.get("indexed_longitude", float("inf")) - immutable["longitude"]) > 1e-10 or \
                abs(result.get("indexed_latitude", float("inf")) - immutable["latitude"]) > 1e-10:
            raise ValueError("An existing basin outlet or source polygon changed during course reassessment")
        result["course_revalidated_utc"] = result["retrieved_utc"]
        result["retrieved_utc"] = previous["retrieved_utc"]
        result["immutable_source_cache_folder"] = str(old_folder)
        (folder / "manifest.json").write_text(json.dumps(result, indent=2), encoding="utf-8")
    return result


if __name__ == "__main__":
    import os as _workflow_os
    _workflow_os.chdir(Path(__file__).resolve().parents[1])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--requests", required=True)
    parser.add_argument("--previous-manifest", required=True)
    parser.add_argument("--cache-dir", default="spatial_data/upstream_basins/batch")
    parser.add_argument("--workers", type=int, choices=range(1, 5), default=3)
    args = parser.parse_args()
    requests = json.loads(Path(args.requests).read_text(encoding="utf-8"))
    prior = json.loads(Path(args.previous_manifest).read_text(encoding="utf-8"))
    previous = {row["review_record_key"]: row for row in prior}
    cache = Path(args.cache_dir)
    results = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
        tasks = [pool.submit(migrate, request, previous[request["review_record_key"]], cache) for request in requests]
        for task in concurrent.futures.as_completed(tasks):
            result = task.result()
            results.append(result)
            (cache / "manifest.json").write_text(json.dumps(results, indent=2), encoding="utf-8")
            print(f"{len(results)}/{len(requests)}: {result['review_number']} {result['status']}", flush=True)
