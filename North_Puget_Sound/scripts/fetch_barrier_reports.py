"""Cache public reports without edits to review outputs.

Dependency: pypdf. Install into an isolated cache if it is unavailable:
  python -m pip install --target spatial_data/barrier_reports/python_deps pypdf
Then run this script, followed by Rscript Build_Barrier_Metadata.R.
Existing PDFs are reused. Pass --refresh explicitly to download them again.
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

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--refresh", action="store_true", help="Explicitly redownload existing PDFs")
parser.add_argument("--workers", type=int, default=4, choices=range(1, 5))
arguments = parser.parse_args()
import os as _workflow_os
_workflow_os.chdir(Path(__file__).resolve().parents[1])
for dependency_path in [Path("spatial_data/barrier_reports/python_deps"),
                        Path(os.environ["TEMP"]) / "habitat_pdf_parser"]:
    if dependency_path.exists():
        sys.path.insert(0, str(dependency_path.resolve()))
try:
    from pypdf import PdfReader
except ImportError as error:
    raise SystemExit("Install pypdf using the isolated-cache command in this script's docstring") from error

cache = Path("spatial_data/barrier_reports").resolve()
if not cache.is_relative_to(Path.cwd().resolve()):
    raise RuntimeError("Report cache must remain in the workspace")
cache.mkdir(parents=True, exist_ok=True)
with open("outputs/review_swifd_connections/barrier_review.csv", newline="", encoding="utf-8-sig") as handle:
    rows = [row for row in csv.DictReader(handle)
            if row["stream_identity_confirmed"].upper() == "TRUE"
            and row["FeatureType"] == "Natural Barrier"]
if len(rows) != 691:
    raise RuntimeError(f"Expected 691 green natural barriers, found {len(rows)}")


def fetch(row):
    site = row["SiteId"]
    url = row["FormLinkURL"]
    pdf = cache / f"{site}_Report.pdf"
    text_file = cache / f"{site}_Report.text.json"
    metadata = dict(SiteId=site, review_number=int(row["review_number"]), report_url=url,
                    report_pdf=str(pdf.relative_to(Path.cwd().resolve())),
                    report_retrieved_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                    report_source_md5=None, report_download_status="not_attempted", pages=[])
    try:
        cached_temp = Path(os.environ["TEMP"]) / f"habitat_barrier_{site}_Report.pdf"
        if not arguments.refresh and not pdf.exists() and cached_temp.exists():
            shutil.copy2(cached_temp, pdf)
        if arguments.refresh or not pdf.exists():
            partial = pdf.with_suffix(".pdf.part")
            result = subprocess.run(
                ["curl.exe", "--ssl-no-revoke", "--fail", "--silent", "--show-error", "--location",
                 "--retry", "2", "--retry-delay", "1", "--connect-timeout", "15", "--max-time", "60",
                 url, "--output", str(partial)], capture_output=True, text=True, timeout=190)
            if result.returncode:
                metadata["report_download_status"] = "download_failed"
                metadata["report_error"] = result.stderr.strip()
                return metadata
            partial.replace(pdf)
        metadata["report_retrieved_utc"] = datetime.datetime.fromtimestamp(
            pdf.stat().st_mtime, datetime.timezone.utc).isoformat()
        raw = pdf.read_bytes()
        if not raw.startswith(b"%PDF"):
            raise ValueError("Downloaded content is not a PDF")
        metadata["report_source_md5"] = hashlib.md5(raw).hexdigest()
        metadata["pages"] = [page.extract_text() or "" for page in PdfReader(pdf).pages]
        metadata["report_download_status"] = "downloaded_and_text_extracted"
    except Exception as error:
        metadata["report_download_status"] = "parse_or_download_error"
        metadata["report_error"] = str(error)
    finally:
        text_file.write_text(json.dumps(metadata, ensure_ascii=False, indent=2), encoding="utf-8")
    return metadata


records = []
with concurrent.futures.ThreadPoolExecutor(max_workers=arguments.workers) as pool:
    futures = {pool.submit(fetch, row): row["SiteId"] for row in rows}
    for future in concurrent.futures.as_completed(futures):
        record = future.result()
        records.append(record)
        if len(records) % 50 == 0 or len(records) == len(rows):
            errors = sum(record["report_download_status"] != "downloaded_and_text_extracted" for record in records)
            print(f"Reports {len(records)}/{len(rows)}; failures={errors}", flush=True)
records.sort(key=lambda record: record["review_number"])
(cache / "report_text_index.json").write_text(
    json.dumps(records, ensure_ascii=False, indent=2), encoding="utf-8")
print("Report cache complete:", len(records), "records", flush=True)
