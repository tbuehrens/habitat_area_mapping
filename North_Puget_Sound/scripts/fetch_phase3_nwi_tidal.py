"""Cache authoritative FWS NWI freshwater-tidal channels and source vintages.

This is a separate comparison cache. It never rewrites CFS or scenario outputs.
"""
import argparse
import datetime
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import urllib.parse

ROOT = "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services"
BBOX = [-123.2, 47.2, -120.5, 49.5]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--refresh", action="store_true")
    args = parser.parse_args()
    cache = Path(__file__).resolve().parents[1] / "spatial_data/phase3/nwi_tidal"
    cache.mkdir(parents=True, exist_ok=True)
    curl = shutil.which("curl.exe") or shutil.which("curl")
    files = []

    def fetch(url, name):
        path = cache / name
        if args.refresh or not path.exists():
            partial = path.with_suffix(path.suffix + ".part")
            command = [curl] + (["--ssl-no-revoke"] if Path(curl).name.lower() == "curl.exe" else [])
            command += ["--fail", "--silent", "--show-error", "--location", "--retry", "2",
                        "--connect-timeout", "20", "--max-time", "180", url, "--output", str(partial)]
            subprocess.run(command, capture_output=True, check=True)
            partial.replace(path)
        content = path.read_bytes()
        data = json.loads(content)
        if "error" in data:
            raise RuntimeError(f"Source API error in {name}: {data['error']}")
        files.append(dict(file=name, source_url=url, bytes=len(content),
                          md5=hashlib.md5(content).hexdigest(), sha256=hashlib.sha256(content).hexdigest(),
                          cached_modified_utc=datetime.datetime.fromtimestamp(path.stat().st_mtime,
                              datetime.timezone.utc).isoformat()))
        return data

    layers = []
    for label, endpoint, where in [("r1", "Wetlands/MapServer/0", "ATTRIBUTE LIKE 'R1%'"),
                                   ("source_vintage", "Data_Source/MapServer/3", "1=1")]:
        base = ROOT + "/" + endpoint
        metadata = fetch(base + "?f=pjson", label + "_metadata.json")
        scope = dict(where=where, geometry=",".join(map(str, BBOX)), geometryType="esriGeometryEnvelope",
                     inSR=4326, spatialRel="esriSpatialRelIntersects", f="json")
        ids_data = fetch(base + "/query?" + urllib.parse.urlencode(dict(scope, returnIdsOnly="true")),
                         label + "_ids.json")
        count_data = fetch(base + "/query?" + urllib.parse.urlencode(dict(scope, returnCountOnly="true")),
                           label + "_count.json")
        ids = sorted(ids_data.get("objectIds") or [])
        if len(ids) != count_data["count"] or len(ids) != len(set(ids)):
            raise RuntimeError("Independent source count and ID inventory disagree")
        print(label, "count", len(ids), flush=True)
        oid = ids_data.get("objectIdFieldName", "OBJECTID")
        batch_size = min(250, metadata["maxRecordCount"])
        batches = []
        for n, start in enumerate(range(0, len(ids), batch_size), 1):
            batch = ids[start:start + batch_size]
            name = f"{label}_batch_{n:04d}.geojson"
            url = base + "/query?" + urllib.parse.urlencode(dict(objectIds=",".join(map(str, batch)),
                outFields="*", returnGeometry="true", returnZ="false", returnM="false", outSR=4326, f="geojson"))
            data = fetch(url, name)
            got = [feature["properties"][oid] for feature in data.get("features", [])]
            if sorted(got) != batch or data.get("exceededTransferLimit", False):
                raise RuntimeError(f"Incomplete source batch {name}")
            batches.append(name)
        layers.append(dict(label=label, endpoint=base, where=where, count=len(ids), oid_field=oid,
                           batch_files=batches, metadata_file=label + "_metadata.json"))
    manifest = dict(query_bbox=BBOX, generated_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                    sources=files, layers=layers,
                    classification_definition_url="https://www.fws.gov/apps/sites/default/files/documents/data-collection-requirements-and-procedures-for-mapping-wetland-deepwater-and-related-habitats-of-the-united-states.pdf",
                    classification_note="R1: freshwater riverine tidal subsystem; not merely estuarine salinity or floodplain.")
    (cache / "source_manifest.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print("Complete NWI R1 and source-vintage snapshots cached", flush=True)


if __name__ == "__main__":
    main()
