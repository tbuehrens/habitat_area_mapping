"""Cache and validate the two Woods Creek natural-barrier records for Phase3.

Run from repository root or the regional folder:
  python North_Puget_Sound/scripts/prepare_phase3_woods_barriers.py
Requires project R dependencies and pypdf (optionally installed with
python -m pip install --target North_Puget_Sound/spatial_data/phase3/python_deps pypdf).
Source snapshots are reused; --refresh explicitly updates public downloads.
Earlier review/Phase1/Phase2 outputs remain read only.
"""
import argparse
import concurrent.futures
import csv
import datetime
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import urllib.parse


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rscript")
    parser.add_argument("--refresh", action="store_true")
    args = parser.parse_args()
    region = Path(__file__).resolve().parents[1]
    os.chdir(region)
    cache = Path("spatial_data/phase3/woods")
    for path in [Path("spatial_data/phase3/python_deps"), Path("spatial_data/phase2/python_deps"),
                 Path(os.environ.get("TEMP", "/tmp")) / "habitat_pdf_parser"]:
        if path.exists():
            sys.path.insert(0, str(path.resolve()))
    try:
        from pypdf import PdfReader
    except ImportError as error:
        raise SystemExit("Install pypdf using this script's documented workspace-cache command") from error
    rscript = args.rscript or shutil.which("Rscript")
    if not rscript:
        candidates = sorted(Path("C:/Program Files/R").glob("R-*/bin/Rscript.exe"))
        if not candidates:
            raise SystemExit("Supply --rscript with the installed Rscript executable")
        rscript = str(candidates[-1])

    def fetch(url, path):
        path.parent.mkdir(parents=True, exist_ok=True)
        if path.exists() and not args.refresh:
            return
        partial = path.with_suffix(path.suffix + ".part")
        command = ["curl.exe" if os.name == "nt" else "curl"]
        if os.name == "nt":
            command.append("--ssl-no-revoke")
        command += ["--fail", "--silent", "--show-error", "--location", "--retry", "2",
                    "--retry-all-errors", "--retry-delay", "1", "--connect-timeout", "15",
                    "--max-time", "60", url, "--output", str(partial)]
        subprocess.run(command, check=True)
        partial.replace(path)

    base = "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/"
    where = ("(UPPER(StreamName) LIKE '%WOOD%' OR UPPER(TributaryToName) LIKE '%WOOD%' "
             "OR UPPER(StreamName) LIKE '%CARPENTER%') AND SiteLatitude BETWEEN 47.80 AND 48.05 "
             "AND SiteLongitude BETWEEN -122.10 AND -121.70")

    def query(layer):
        url = base + str(layer) + "/query?" + urllib.parse.urlencode(dict(
            where=where, outFields="*", outSR=4326, returnGeometry="true", f="geojson"))
        path = cache / "source_queries" / f"layer_{layer}.geojson"
        fetch(url, path)
        data = json.loads(path.read_text(encoding="utf-8-sig"))
        if "error" in data or data.get("exceededTransferLimit"):
            raise RuntimeError(f"Incomplete WDFW Woods query at layer{layer}")
        return dict(layer=layer, source_url=url, cache_file=str(path),
                    md5=hashlib.md5(path.read_bytes()).hexdigest(), feature_count=len(data["features"]),
                    retrieved_utc=datetime.datetime.fromtimestamp(path.stat().st_mtime, datetime.timezone.utc).isoformat())

    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        queries = list(pool.map(query, range(9)))
    (cache / "source_queries" / "query_manifest.json").write_text(json.dumps(queries, indent=2), encoding="utf-8")
    inventory = []
    for query_record in queries:
        data = json.loads(Path(query_record["cache_file"]).read_text(encoding="utf-8-sig"))
        for feature in data["features"]:
            values = feature["properties"]
            direct = str(values.get("StreamName") or "").lower() in ("woods cr", "wf woods cr", "ef woods cr")
            inventory.append(dict(layer=query_record["layer"], SiteId=values.get("SiteId"),
                StreamName=values.get("StreamName"), TributaryToName=values.get("TributaryToName"),
                FeatureType=values.get("FeatureType"), PercentFishPassableCode=values.get("PercentFishPassableCode"),
                directly_named_Woods_fork=direct, review_scope=("Named Woods fork" if direct else
                "Separate tributary, Carpenter headwater or other WoodsLake/Sultan drainage; no fork-cut inference")))
    with (cache / "source_queries" / "query_inventory.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(inventory[0]))
        writer.writeheader()
        writer.writerows(inventory)
    if any(row["layer"] == 2 and row["directly_named_Woods_fork"] for row in inventory):
        raise RuntimeError("A Total-coded manmade barrier now appears on a named Woods fork; review this additional source before rebuilding")

    for number, site in [(10, "891970"), (914, "891169")]:
        url = f"https://fortress.wa.gov/dfw/public/fishpassagephotos/reports/{site}_Report.pdf"
        pdf = cache / "reports" / f"{site}_Report.pdf"
        fetch(url, pdf)
        raw = pdf.read_bytes()
        if not raw.startswith(b"%PDF"):
            raise RuntimeError("Source report is not PDF: " + str(pdf))
        record = dict(review_number=number, SiteId=site, report_url=url, report_pdf=str(pdf),
            report_retrieved_utc=datetime.datetime.fromtimestamp(pdf.stat().st_mtime, datetime.timezone.utc).isoformat(),
            report_source_md5=hashlib.md5(raw).hexdigest(), report_download_status="downloaded_and_text_extracted",
            pages=[page.extract_text() or "" for page in PdfReader(pdf).pages])
        pdf.with_suffix(".text.json").write_text(json.dumps(record, indent=2), encoding="utf-8")
    sources = [dict(title="WDFW verified natural barrier891970", review_number=10,
                   source_url="https://fortress.wa.gov/dfw/public/fishpassagephotos/reports/891970_Report.pdf", page=3),
               dict(title="WDFW unverified natural barrier891169", review_number=914,
                   source_url="https://fortress.wa.gov/dfw/public/fishpassagephotos/reports/891169_Report.pdf", page=3),
               dict(title="Ecology WRIA7 Snohomish Watershed Plan2024, Woods Creek project description", page="C-88",
                   source_url="https://apps.ecology.wa.gov/publications/parts/2211013part1.pdf",
                   finding="EastFork has an anadromous-passage waterfall atRM4.2; the forks join atRM3.9. WestFork headwaters are calledCarpenter Creek.")]
    (cache / "authoritative_sources.json").write_text(json.dumps(sources, indent=2), encoding="utf-8")

    names = ["project_paths", "source_classification", "barrier_stream_identity", "stream_edge_identity",
             "network", "barrier_metadata", "phase_barrier_cuts", "upstream_basins", "nldi_delineation",
             "phase3_woods_barriers"]
    source = "\n".join(f"source('R/{name}.R')" for name in names)
    subprocess.run([rscript, "-"], input=source + "\nbuild_phase3_woods_barriers(prepare_only=TRUE)\n",
                   text=True, check=True)
    batch = cache / "basins"
    command = [sys.executable, "scripts/fetch_upstream_basins.py", "--requests", str(batch / "requests.json"),
               "--cache-dir", str(batch), "--workers", "1"]
    if args.refresh:
        command.append("--refresh")
    subprocess.run(command, check=True)
    fetch("https://api.water.usgs.gov/nldi/linked-data/comid/23963775/navigation/UT/flowlines?f=json&distance=1000",
          cache / "upstream_navigation.json")
    fetch("https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdflowline_network/items?comid=23963905&f=json",
          cache / "northern_upstream_reference.json")
    subprocess.run([rscript, "-"], input=source + "\nbuild_phase3_woods_barriers()\n", text=True, check=True)
    print("Woods bundle validated: spatial_data/phase3/woods_barriers.rds;10applied,914held beyond50m", flush=True)


if __name__ == "__main__":
    main()
