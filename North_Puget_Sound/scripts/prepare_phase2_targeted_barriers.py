"""Prepare the two individually reviewed Phase 2 barrier caches and R bundle.

Run from North_Puget_Sound:
  python scripts/prepare_phase2_targeted_barriers.py
Requires the project R dependencies and pypdf. To install pypdf in the workspace:
  python -m pip install --target spatial_data/phase2/python_deps pypdf
Existing public-source downloads are reused unless --refresh is specified.
Phase 1 and untruncated review outputs are read only throughout.
"""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rscript", help="Path to Rscript.exe if absent from PATH")
    parser.add_argument("--refresh", action="store_true")
    args = parser.parse_args()
    root = Path.cwd().resolve()
    cache = root / "spatial_data/phase2"
    for path in [cache / "python_deps", Path(os.environ.get("TEMP", "/tmp")) / "habitat_pdf_parser"]:
        if path.exists():
            sys.path.insert(0, str(path))
    try:
        from pypdf import PdfReader
    except ImportError as error:
        raise SystemExit("Install pypdf with the workspace-cache command in this script's docstring") from error
    rscript = args.rscript or shutil.which("Rscript")
    if not rscript:
        candidates = sorted(Path("C:/Program Files/R").glob("R-*/bin/Rscript.exe"))
        if not candidates:
            raise SystemExit("Supply --rscript with the installed Rscript executable")
        rscript = str(candidates[-1])

    def fetch(url, path, payload=None):
        path.parent.mkdir(parents=True, exist_ok=True)
        if path.exists() and not args.refresh:
            return
        partial = path.with_suffix(path.suffix + ".part")
        command = ["curl.exe", "--ssl-no-revoke", "--fail", "--silent", "--show-error", "--location",
                   "--retry", "2", "--retry-all-errors", "--retry-delay", "1", "--connect-timeout", "15",
                   "--max-time", "60"]
        if payload is not None:
            request_file = path.with_suffix(".request.json")
            request_file.write_text(json.dumps(payload, indent=2), encoding="utf-8")
            command += ["-H", "Content-Type: application/json", "--data-binary", "@" + str(request_file)]
        command += [url, "--output", str(partial)]
        subprocess.run(command, check=True)
        partial.replace(path)

    for number, site in [(663, "890769"), (344, "891098")]:
        url = f"https://fortress.wa.gov/dfw/public/fishpassagephotos/reports/{site}_Report.pdf"
        pdf = cache / "reports" / f"{site}_Report.pdf"
        fetch(url, pdf)
        raw = pdf.read_bytes()
        if not raw.startswith(b"%PDF"):
            raise RuntimeError("Public report is not PDF: " + str(pdf))
        record = dict(review_number=number, SiteId=site, report_url=url,
                      report_pdf=str(pdf.relative_to(root)), report_source_md5=hashlib.md5(raw).hexdigest(),
                      report_retrieved_utc=datetime.datetime.fromtimestamp(pdf.stat().st_mtime, datetime.timezone.utc).isoformat(),
                      report_download_status="downloaded_and_text_extracted",
                      pages=[page.extract_text() or "" for page in PdfReader(pdf).pages])
        pdf.with_suffix(".text.json").write_text(json.dumps(record, indent=2), encoding="utf-8")

    source = "\n".join(
        f"source('R/{name}.R')" for name in ["arcgis", "source_classification", "barrier_stream_identity",
        "stream_edge_identity", "stream_reference", "network", "barrier_metadata", "phase_barrier_cuts",
        "upstream_basins", "nldi_delineation", "phase2_targeted_barriers"])
    subprocess.run([rscript, "-"], input=source + "\nbuild_phase2_targeted_barriers(prepare_only=TRUE)\n",
                   text=True, check=True)
    batch = cache / "targeted_basins"
    command = [sys.executable, "scripts/fetch_upstream_basins.py", "--requests", str(batch / "requests.json"),
               "--cache-dir", str(batch), "--workers", "2"]
    if args.refresh:
        command.append("--refresh")
    subprocess.run(command, check=True)
    requests = json.loads((batch / "requests.json").read_text())
    records = json.loads((batch / "manifest.json").read_text())
    physical = next(row for row in requests if row["review_number"] == 344)
    indexed = next(row for row in records if row["review_number"] == 344)
    if indexed.get("comid") != "23963587" or indexed.get("route_support_coverage", 0) < 1:
        raise RuntimeError("West Cady published same-channel assignment changed; review before proceeding")
    recovery = cache / "west_cady_recovery"
    endpoint = "https://api.water.usgs.gov/nldi/pygeoapi/processes/"

    def inputs(**values):
        return {"inputs": [dict(id=key, type="text/plain", value=str(value)) for key, value in values.items()]}

    for label, longitude, latitude in [("physical", physical["longitude"], physical["latitude"]),
        ("indexed", indexed["indexed_longitude"], indexed["indexed_latitude"])]:
        fetch(endpoint + "nldi-flowtrace/execution?f=json", recovery / f"{label}_flowtrace.json",
              inputs(lat=latitude, lon=longitude, direction="none"))
    trace = json.loads((recovery / "physical_flowtrace.json").read_text())
    feature = next(item for item in trace["features"] if item.get("id") == "nhdFlowline")
    if str(feature["properties"].get("comid")) != "23963587":
        raise RuntimeError("Official flowtrace changed the reviewed West Cady stream")
    longitude, latitude = feature["properties"]["intersection_point"]
    fetch(endpoint + "nldi-splitcatchment/execution?f=json", recovery / "flowtraced_split_basin.json",
          inputs(lat=latitude, lon=longitude, upstream="true", simplified="false"))
    fetch("https://api.water.usgs.gov/nldi/linked-data/comid/23963587/navigation/UT/flowlines?f=json&distance=1000",
          recovery / "upstream_flowlines.json")
    subprocess.run([rscript, "-"], input=source + "\nbuild_phase2_targeted_barriers()\n", text=True, check=True)
    print("Validated two-site bundle: spatial_data/phase2/targeted_barriers.rds", flush=True)


if __name__ == "__main__":
    import os as _workflow_os
    _workflow_os.chdir(Path(__file__).resolve().parents[1])
    main()
