"""Prepare the reviewed lower French Creek Total barrier without changing outputs.

Run from the repository root or regional folder. Existing source snapshots are
reused; --refresh explicitly downloads current public sources again.
"""
import argparse
import concurrent.futures
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
    parser.add_argument('--rscript')
    parser.add_argument('--refresh', action='store_true')
    parser.add_argument('--sources-only', action='store_true')
    args = parser.parse_args()
    region = Path(__file__).resolve().parents[1]
    os.chdir(region)
    cache = Path('spatial_data/stillaguamish_review/french')
    for directory in [Path('spatial_data/phase3/python_deps'), Path('spatial_data/phase2/python_deps')]:
        if directory.exists():
            sys.path.insert(0, str(directory.resolve()))
    try:
        from pypdf import PdfReader
        def pdf_pages(path):
            return [page.extract_text() or '' for page in PdfReader(path).pages]
    except ImportError:
        import pymupdf
        def pdf_pages(path):
            with pymupdf.open(path) as document:
                return [page.get_text() for page in document]

    def fetch(url, path, payload=None):
        path.parent.mkdir(parents=True, exist_ok=True)
        if path.exists() and not args.refresh:
            return
        partial = path.with_suffix(path.suffix + '.part')
        command = ['curl.exe' if os.name == 'nt' else 'curl']
        if os.name == 'nt':
            command += ['--ssl-no-revoke']
        command += ['--fail', '--silent', '--show-error', '--location', '--retry', '2',
                    '--retry-delay', '1', '--connect-timeout', '15', '--max-time', '60']
        if payload is not None:
            request = path.with_suffix('.request.json')
            request.write_text(json.dumps(payload, indent=2), encoding='utf-8')
            command += ['--header', 'Content-Type: application/json', '--data-binary', '@' + str(request)]
        subprocess.run(command + [url, '--output', str(partial)], check=True)
        partial.replace(path)

    def inputs(**values):
        return dict(inputs=[dict(id=k, type='text/plain', value=str(v)) for k, v in values.items()])

    # All fish-passage layers, including structures, are inspected. Nearby
    # tributaries remain in the inventory and never authorize a French cut.
    base = 'https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/'
    where = ("(UPPER(StreamName) LIKE '%FRENCH%' OR UPPER(TributaryToName) LIKE '%FRENCH%') "
             'AND SiteLatitude BETWEEN 48.20 AND 48.31 AND SiteLongitude BETWEEN -121.90 AND -121.65')

    def query(layer):
        url = base + str(layer) + '/query?' + urllib.parse.urlencode(dict(
            where=where, outFields='*', outSR=4326, returnGeometry='true', f='geojson'))
        path = cache / 'source_queries' / f'layer_{layer}.geojson'
        fetch(url, path)
        data = json.loads(path.read_text(encoding='utf-8-sig'))
        if 'error' in data or data.get('exceededTransferLimit'):
            raise RuntimeError('Incomplete French Creek source inventory')
        return dict(layer=layer, source_url=url, cache_file=str(path),
                    md5=hashlib.md5(path.read_bytes()).hexdigest(), feature_count=len(data['features']),
                    retrieved_utc=datetime.datetime.fromtimestamp(path.stat().st_mtime, datetime.timezone.utc).isoformat())

    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        records = list(pool.map(query, range(9)))
    (cache / 'source_queries' / 'query_manifest.json').write_text(json.dumps(records, indent=2), encoding='utf-8')
    for number, site in [(563, '890795'), (581, '890651'), (915, '890155')]:
        url = f'https://fortress.wa.gov/dfw/public/fishpassagephotos/reports/{site}_Report.pdf'
        pdf = cache / 'reports' / f'{site}_Report.pdf'
        fetch(url, pdf)
        if not pdf.read_bytes().startswith(b'%PDF'):
            raise RuntimeError('Source report is not a PDF')
        report = dict(review_number=number, SiteId=site, report_url=url, report_pdf=str(pdf),
                      report_source_md5=hashlib.md5(pdf.read_bytes()).hexdigest(),
                      report_retrieved_utc=datetime.datetime.fromtimestamp(pdf.stat().st_mtime, datetime.timezone.utc).isoformat(),
                      report_download_status='downloaded_and_text_extracted',
                      pages=pdf_pages(pdf))
        pdf.with_suffix('.text.json').write_text(json.dumps(report, indent=2), encoding='utf-8')

    manifest = json.loads(Path('spatial_data/upstream_basins/batch/manifest.json').read_text(encoding='utf-8-sig'))
    existing = next(row for row in manifest if row['review_number'] == 581)
    if existing['comid'] != '24274901' or existing['reference_stream_name'] != 'French Creek':
        raise RuntimeError('Reviewed French Creek reference changed')
    endpoint = 'https://api.water.usgs.gov/nldi/pygeoapi/processes/'
    recovery = cache / 'recovery'
    for label, longitude, latitude in [
        ('physical', existing['longitude'], existing['latitude']),
        ('indexed', existing['indexed_longitude'], existing['indexed_latitude'])]:
        fetch(endpoint + 'nldi-flowtrace/execution?f=json', recovery / f'{label}_flowtrace.json',
              inputs(lon=longitude, lat=latitude, direction='none'))
    trace = json.loads((recovery / 'physical_flowtrace.json').read_text(encoding='utf-8-sig'))
    feature = next(f for f in trace['features'] if f.get('id') == 'nhdFlowline')
    if str(feature['properties'].get('comid')) != '24274901':
        raise RuntimeError('Official flowtrace identifies a different stream')
    longitude, latitude = feature['properties']['intersection_point']
    fetch(endpoint + 'nldi-splitcatchment/execution?f=json', recovery / 'flowtraced_split_basin.json',
          inputs(lon=longitude, lat=latitude, upstream='true', simplified='false'))
    fetch('https://api.water.usgs.gov/nldi/linked-data/comid/24274901/navigation/UT/flowlines?f=json&distance=1000',
          recovery / 'upstream_flowlines.json')
    if args.sources_only:
        return
    rscript = args.rscript or shutil.which('Rscript')
    if not rscript:
        rscript = str(sorted(Path('C:/Program Files/R').glob('R-*/bin/Rscript.exe'))[-1])
    names = ['project_paths', 'source_classification', 'barrier_stream_identity', 'stream_edge_identity',
             'network', 'barrier_metadata', 'phase_barrier_cuts', 'upstream_basins', 'nldi_delineation',
             'french_creek_review']
    source = '\n'.join(f"source('R/{name}.R')" for name in names)
    subprocess.run([rscript, '-'], input=source + '\nbuild_french_creek_review()\n', text=True, check=True)
    print('French Creek source evidence and complete-basin bundle prepared', flush=True)


if __name__ == '__main__':
    main()
