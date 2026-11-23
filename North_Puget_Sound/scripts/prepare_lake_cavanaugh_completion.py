"""Prepare the explicit Lake Cavanaugh correction for Pilchuck Creek waterfall634.

Run after prepare_pilchuck_creek_review.py. This caches original authoritative
source components; --build regenerates independent fine-channel evidence and
the validated review bundle. It does not run exports or modify raw habitat.
The NHD lake's two ArtificialPaths share named waterbody24277115 but receive
inconsistent downstream routing. Their complete source-basin union is accepted
only after the R builder inventories every newly intersected source segment,
proves its actual lake location, and excludes genuine fine-reference Deer Creek.
"""
from pathlib import Path
import argparse
import datetime
import json
import os
import shutil
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build', action='store_true')
    parser.add_argument('--rscript')
    arguments = parser.parse_args()
    directory = Path('spatial_data/lake_cavanaugh_review')
    directory.mkdir(parents=True, exist_ok=True)
    old = Path('spatial_data/stillaguamish_review')
    fabric = 'https://api.water.usgs.gov/fabric/pygeoapi/collections/'
    sources = [
        ('nhd_waterbody_24277115.json',
         old/'lake_cavanaugh_audit/nhd_waterbody_24277115.json',
         fabric+'nhdwaterbody/items?comid=24277115&f=json'),
        ('lake_cavanaugh_24275741_whole_basin.json',
         old/'pilchuck_sources/foreign_24275741_whole_basin.json',
         'https://api.water.usgs.gov/nldi/linked-data/comid/24275741/basin?f=json&simplified=false&splitCatchment=false'),
        ('nhd_flowline_24274873.json',
         old/'pilchuck_sources/lake_cavanaugh_24274873_flowline.json',
         fabric+'nhdflowline_network/items?comid=24274873&f=json'),
        ('nhd_flowline_24275741.json',
         old/'pilchuck_sources/lake_creek_missing_24275741_flowline.json',
         fabric+'nhdflowline_network/items?comid=24275741&f=json'),
    ]
    for name, previous, url in sources:
        destination = directory/name
        if not destination.exists():
            if previous.exists():
                shutil.copy2(previous, destination)
            else:
                temporary = destination.with_suffix('.part')
                subprocess.run(['curl.exe', '--ssl-no-revoke', '--fail', '--silent',
                                '--show-error', '--location', '--connect-timeout', '20',
                                '--max-time', '120', '--output', str(temporary), url], check=True)
                json.loads(temporary.read_text())
                temporary.replace(destination)
        json.loads(destination.read_text())

    # The direct Ecology PDF endpoint returned403 during the2026-11-21 review.
    # Keep the honestly identified browser-extracted20-word passage and primary
    # WAC receiver evidence; never manufacture a downloaded PDF or its hash.
    route = directory/'official_receiving_route.json'
    if not route.exists():
        route.write_text(json.dumps(dict(
            source_url='https://appswr.ecology.wa.gov/docs/WaterRights/ScanToWRTS/HQ4/6802520_DraftReportofExam_50_156.pdf',
            title='Ecology draft Report of Examination S1-28916, WR Doc ID6802520',
            page=5,
            exact_excerpt='From the outlet weir, water is released to Lake Creek, tributary to Pilchuck Creek, which flows into the Stillaguamish River.',
            retrieval_method='Browser PDF text extraction2026-11-21, page5 lines168-171; local direct PDF download failed403',
            pdf_archived=False,
            corroborating_primary_url='https://app.leg.wa.gov/wac/default.aspx?cite=173-18-330',
            corroborating_primary_entry='27; Lake Creek extends from Lake Cavanaugh outlet downstream to Pilchuck Creek',
            metadata_saved_utc=datetime.datetime.now(datetime.timezone.utc).isoformat()), indent=2))

    if arguments.build:
        rscript = arguments.rscript or shutil.which('Rscript')
        if not rscript:
            rscript = str(sorted(Path('C:/Program Files/R').glob('R-*/bin/Rscript.exe'))[-1])
        subprocess.run([rscript, 'scripts/audit_lake_cavanaugh_channel.R'], check=True)
        modules = ['arcgis', 'source_classification', 'barrier_metadata',
                   'barrier_stream_identity', 'stream_edge_identity', 'stream_reference',
                   'network', 'phase_barrier_cuts', 'upstream_basins',
                   'nldi_delineation', 'pilchuck_creek_review']
        source = '\n'.join(f"source('R/{module}.R')" for module in modules)
        subprocess.run([rscript, '-'], input=source+'\nbuild_pilchuck_creek_review()\n',
                       text=True, check=True)
        subprocess.run([rscript, 'tests/test_pilchuck_creek_review.R'], check=True)
    print('Lake Cavanaugh source components ready; no published output was changed.')


if __name__ == '__main__':
    os.chdir(Path(__file__).resolve().parents[1])
    main()
