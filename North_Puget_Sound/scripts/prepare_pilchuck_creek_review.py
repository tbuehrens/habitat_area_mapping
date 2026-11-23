"""Cache current official Pilchuck CREEK records and precise flowtrace evidence.
Older maps, tables and source snapshots are read only. Run from any directory.
"""
import json
from pathlib import Path
import os
import subprocess
import urllib.parse
import sys
import hashlib
import datetime
import argparse
import shutil

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build',action='store_true',help='Build and validate the R review bundle after preparing sources')
    parser.add_argument('--rscript')
    arguments=parser.parse_args()
    cache=Path('spatial_data/stillaguamish_review/pilchuck_sources')
    cache.mkdir(parents=True,exist_ok=True)
    def fetch(url,path,payload=None):
        if path.exists(): return json.loads(path.read_text()) if path.suffix=='.json' else None
        command=['curl.exe','--ssl-no-revoke','--fail','--silent','--show-error','--location',
            '--connect-timeout','20','--max-time','120','--output',str(path.with_suffix('.part'))]
        if payload is not None:
            req=path.with_suffix('.request.json');req.write_text(json.dumps(payload,indent=2))
            command+=['--header','Content-Type: application/json','--data-binary','@'+str(req)]
        subprocess.run(command+[url],check=True)
        path.with_suffix('.part').replace(path)
        return json.loads(path.read_text()) if path.suffix=='.json' else None
    query=urllib.parse.urlencode(dict(where="SiteId IN ('890624','890145','890682')",outFields='*',outSR=4326,f='json'))
    fetch('https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/6/query?'+query,
        cache/'current_wdfw_sites.json')
    for site in ['890624','890145','890682']:
        fetch(f'https://fortress.wa.gov/dfw/public/fishpassagephotos/reports/{site}_Report.pdf',cache/f'{site}_Report.pdf')
    for dependency in [Path('spatial_data/phase2/python_deps'),Path(os.environ.get('TEMP','/tmp'))/'habitat_pdf_parser']:
        if dependency.exists():sys.path.insert(0,str(dependency.resolve()))
    from pypdf import PdfReader
    for site in ['890624','890145','890682']:
        pdf=cache/f'{site}_Report.pdf'
        record=dict(SiteId=site,report_pdf=str(pdf),
            report_url=f'https://fortress.wa.gov/dfw/public/fishpassagephotos/reports/{site}_Report.pdf',
            report_source_md5=hashlib.md5(pdf.read_bytes()).hexdigest(),
            report_retrieved_utc=datetime.datetime.fromtimestamp(pdf.stat().st_mtime,datetime.timezone.utc).isoformat(),
            pages=[page.extract_text() or '' for page in PdfReader(pdf).pages])
        pdf.with_suffix('.text.json').write_text(json.dumps(record,indent=2))
    rscript=arguments.rscript or shutil.which('Rscript')
    if not rscript:rscript=str(sorted(Path('C:/Program Files/R').glob('R-*/bin/Rscript.exe'))[-1])
    source='\n'.join(f"source('R/{name}.R')" for name in ['arcgis','source_classification',
        'barrier_metadata','barrier_stream_identity','stream_edge_identity','stream_reference',
        'network','phase_barrier_cuts','upstream_basins','nldi_delineation','pilchuck_creek_review'])
    subprocess.run([rscript,'-'],input=source+'\nprepare_pilchuck_creek_requests()\n',text=True,check=True)
    endpoint='https://api.water.usgs.gov/nldi/pygeoapi/processes/'
    def inputs(**values):return dict(inputs=[dict(id=k,type='text/plain',value=str(v)) for k,v in values.items()])
    for number,lon,lat in [(628,-122.161540408565,48.3010910522979),(634,-122.149262930925,48.3173320486407)]:
        trace=fetch(endpoint+'nldi-flowtrace/execution?f=json',cache/f'{number}_physical_flowtrace.json',
            inputs(lon=lon,lat=lat,direction='none'))
        flow=next(f for f in trace['features'] if f.get('id')=='nhdFlowline')
        comid=flow['properties']['comid'];longitude,latitude=flow['properties']['intersection_point']
        print(number,flow['properties'],flush=True)
        fetch(endpoint+'nldi-splitcatchment/execution?f=json',cache/f'{number}_flowtraced_split_basin.json',
            inputs(lon=longitude,lat=latitude,upstream='true',simplified='false'))
        fetch(f'https://api.water.usgs.gov/nldi/linked-data/comid/{comid}/navigation/UT/flowlines?f=json&distance=1000',
            cache/f'{number}_upstream_flowlines.json')
    for label,lon,lat in [('lake_creek_missing',-121.9995,48.31791),('lake_creek_mouth',-122.0573,48.3422),
        ('lake_cavanaugh',-122.012,48.332)]:
        query=urllib.parse.urlencode(dict(coords=f'POINT({lon} {lat})',f='json'))
        position=fetch('https://api.water.usgs.gov/nldi/linked-data/comid/position?'+query,cache/f'{label}_position.json')
        comids={str(f['properties']['comid']) for f in position['features']}
        for comid in comids:
            q=urllib.parse.urlencode(dict(comid=comid,f='json'))
            route=fetch('https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdflowline_network/items?'+q,
                cache/f'{label}_{comid}_flowline.json')
            print('PROBE',label,comid,[f['properties'].get('gnis_name') for f in route['features']],flush=True)
    for comid in ['24275741','24274849']:
        fetch(f'https://api.water.usgs.gov/nldi/linked-data/comid/{comid}/navigation/DM/flowlines?f=json&distance=1000',
            cache/f'{comid}_downstream_flowlines.json')
    fetch('https://api.water.usgs.gov/nldi/linked-data/comid/24275741/basin?f=json&simplified=false&splitCatchment=false',
        cache/'foreign_24275741_whole_basin.json')
    for comid in ['24275071','24274777','24274371']:
        q=urllib.parse.urlencode(dict(comid=comid,f='json'))
        route=fetch('https://api.water.usgs.gov/fabric/pygeoapi/collections/nhdflowline_network/items?'+q,
            cache/f'context_{comid}_flowline.json')
        print('DOWNSTREAM',comid,[f['properties'].get('gnis_name') for f in route['features']],flush=True)
    requests_file=Path('spatial_data/stillaguamish_review/pilchuck_requests.json')
    if requests_file.exists():
        request=json.loads(requests_file.read_text())
        lon,lat=request['vector_longitude'],request['vector_latitude']
        query=urllib.parse.urlencode(dict(coords=f'POINT({lon} {lat})',f='json'))
        fetch('https://api.water.usgs.gov/nldi/linked-data/comid/position?'+query,cache/'634_vector_position.json')
        fetch(endpoint+'nldi-splitcatchment/execution?f=json',cache/'634_vector_split_basin.json',
            inputs(lon=lon,lat=lat,upstream='true',simplified='false'))
    print('Current Pilchuck Creek source caches prepared',flush=True)
    if arguments.build:
        subprocess.run([rscript,'-'],input=source+'\nbuild_pilchuck_creek_review()\n',text=True,check=True)
        print('Validated bundle: spatial_data/stillaguamish_review/pilchuck_creek.rds',flush=True)

if __name__=='__main__':
    os.chdir(Path(__file__).resolve().parents[1]);main()
