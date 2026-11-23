"""Cache the public CFS/WDFW 2025 tidal mapping without changing outputs.

Run from any directory: python North_Puget_Sound/scripts/fetch_phase3_tidal_sources.py
Only the Python standard library and curl are required. Existing snapshots are
reused; --refresh explicitly replaces them. Both complete polygon layers are
inventoried by OBJECTID, fetched in bounded batches, and verified independently.
"""
import argparse
import concurrent.futures
import datetime
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import urllib.parse


SERVICE = "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/HP_Projects/CFS_Tidal_Restrictions_2025/MapServer"
REPORT = "https://wikiworks-salishsea-images.s3.amazonaws.com/main/b/b0/20-1941P_Tidal_Restrictions_FinalReport.pdf"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--refresh", action="store_true")
    parser.add_argument("--workers", type=int, default=3, choices=range(1, 5))
    parser.add_argument("--skip-report", action="store_true")
    args = parser.parse_args()
    region = Path(__file__).resolve().parents[1]
    cache = region / "spatial_data/phase3/tidal_sources"
    cache.mkdir(parents=True, exist_ok=True)
    curl = shutil.which("curl.exe") or shutil.which("curl")
    if not curl:
        raise SystemExit("curl is required for the public source snapshots")

    def fetch(url, path):
        if args.refresh or not path.exists():
            partial = path.with_suffix(path.suffix + ".part")
            command = [curl]
            if Path(curl).name.lower() == "curl.exe":
                command += ["--ssl-no-revoke"]
            command += ["--fail", "--silent", "--show-error", "--location", "--retry", "2",
                        "--retry-delay", "1", "--connect-timeout", "20", "--max-time", "180",
                        url, "--output", str(partial)]
            subprocess.run(command, check=True, capture_output=True)
            partial.replace(path)
        content = path.read_bytes()
        if not content:
            raise RuntimeError(f"Empty source: {path.name}")
        if path.suffix != ".pdf":
            parsed = json.loads(content)
            if "error" in parsed:
                raise RuntimeError(f"ArcGIS returned an error for {path.name}: {parsed['error']}")
        return dict(file=path.relative_to(cache).as_posix(), source_url=url,
                    bytes=len(content), md5=hashlib.md5(content).hexdigest(),
                    sha256=hashlib.sha256(content).hexdigest(),
                    cached_modified_utc=datetime.datetime.fromtimestamp(path.stat().st_mtime,
                        datetime.timezone.utc).isoformat())

    inventory = [fetch(SERVICE + "?f=pjson", cache / "service_metadata.json")]
    layer_inventory = []
    for layer in [2, 3]:
        base = f"{SERVICE}/{layer}"
        metadata_file = cache / f"layer_{layer}_metadata.json"
        ids_file = cache / f"layer_{layer}_ids.json"
        count_file = cache / f"layer_{layer}_count.json"
        inventory.append(fetch(base + "?f=pjson", metadata_file))
        inventory.append(fetch(base + "/query?" + urllib.parse.urlencode(dict(
            where="1=1", returnIdsOnly="true", f="json")), ids_file))
        inventory.append(fetch(base + "/query?" + urllib.parse.urlencode(dict(
            where="1=1", returnCountOnly="true", f="json")), count_file))
        metadata = json.loads(metadata_file.read_text(encoding="utf-8-sig"))
        ids_raw = json.loads(ids_file.read_text(encoding="utf-8-sig"))
        ids = sorted(ids_raw["objectIds"])
        count = json.loads(count_file.read_text(encoding="utf-8-sig"))["count"]
        if len(ids) != len(set(ids)) or len(ids) != count:
            raise RuntimeError(f"Layer {layer}: independent count and unique ID inventory disagree")
        batch_size = min(250, metadata["maxRecordCount"])
        tasks = []
        for index, offset in enumerate(range(0, len(ids), batch_size), 1):
            batch = ids[offset:offset + batch_size]
            url = base + "/query?" + urllib.parse.urlencode(dict(
                objectIds=",".join(map(str, batch)), outFields="*", returnGeometry="true",
                returnZ="false", returnM="false", outSR=4326, f="geojson"))
            path = cache / f"layer_{layer}_batch_{index:04d}.geojson"
            tasks.append((url, path, batch))
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
            records = list(pool.map(lambda task: fetch(task[0], task[1]), tasks))
        fetched_ids = []
        missing_geometry_ids = []
        oid_field = ids_raw["objectIdFieldName"]
        for task in tasks:
            data = json.loads(task[1].read_text(encoding="utf-8-sig"))
            features = data.get("features", [])
            got = [feature["properties"][oid_field] for feature in features]
            if sorted(got) != task[2] or data.get("exceededTransferLimit", False):
                raise RuntimeError(f"Incomplete or duplicate feature batch: {task[1].name}")
            missing_geometry_ids += [feature["properties"][oid_field]
                                     for feature in features if not feature.get("geometry")]
            fetched_ids += got
        if sorted(fetched_ids) != ids:
            raise RuntimeError(f"Layer {layer}: complete batch inventory disagrees with source IDs")
        inventory += records
        # Retain, disclose, and defer a published empty feature rather than
        # silently dropping its attributes or inventing a replacement polygon.
        for missing_id in missing_geometry_ids:
            url = base + "/query?" + urllib.parse.urlencode(dict(objectIds=missing_id,
                outFields="*", returnGeometry="true", outSR=4326, f="json"))
            inventory.append(fetch(url, cache / f"layer_{layer}_feature_{missing_id}_native.json"))
        layer_inventory.append(dict(layer=layer, name=metadata["name"], count=count,
            objectid_field=oid_field, metadata_file=metadata_file.name, ids_file=ids_file.name,
            batch_files=[task[1].name for task in tasks], missing_geometry_ids=missing_geometry_ids))
        print(f"Layer {layer}: {count} unique records, {len(tasks)} verified raw batches; "
              f"{len(missing_geometry_ids)} published empty geometries held", flush=True)
    if not args.skip_report:
        inventory.append(fetch(REPORT, cache / "CFS_2025_Tidal_Restrictions_FinalReport.pdf"))
    manifest = dict(service_url=SERVICE, report_url=REPORT, query_scope="complete published layers 2 and 3",
                    geometry_output_crs="EPSG:4326", service_native_crs="EPSG:2927 (US survey feet)",
                    generated_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                    layers=layer_inventory, files=inventory)
    (cache / "source_manifest.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(f"Source manifest: {cache / 'source_manifest.json'}", flush=True)


if __name__ == "__main__":
    main()
