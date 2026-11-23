"""Prepare precise official Olney source caches; earlier products stay read only.

Run from the repo root or regional directory. Existing sources are reused.
"""
import argparse
import json
from pathlib import Path
import os
import shutil
import subprocess
import sys


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rscript')
    parser.add_argument('--prepare-only',action='store_true')
    arguments=parser.parse_args()
    rscript=arguments.rscript or shutil.which('Rscript')
    if not rscript:
        rscript=str(sorted(Path('C:/Program Files/R').glob('R-*/bin/Rscript.exe'))[-1])
    source='\n'.join(f"source('R/{name}.R')" for name in [
        'arcgis','source_classification','barrier_stream_identity','stream_edge_identity',
        'stream_reference','network','barrier_metadata','phase_barrier_cuts','upstream_basins',
        'nldi_delineation','phase3_olney_barriers'])
    subprocess.run([rscript,'-'],input=source+'\nbuild_phase3_olney_barriers(prepare_only=TRUE)\n',
        text=True,check=True)
    cache=Path('spatial_data/phase3/olney_recovery')
    cache.mkdir(parents=True,exist_ok=True)
    record=json.loads(Path('spatial_data/phase3/olney_inputs.json').read_text())

    def fetch(url,path,payload=None):
        if path.exists():
            return json.loads(path.read_text())
        command=['curl.exe','--ssl-no-revoke','--fail','--silent','--show-error','--location',
            '--connect-timeout','20','--max-time','120','--output',str(path.with_suffix('.part'))]
        if payload is not None:
            request=path.with_suffix('.request.json')
            request.write_text(json.dumps(payload,indent=2))
            command+=['--header','Content-Type: application/json','--data-binary','@'+str(request)]
        subprocess.run(command+[url],check=True)
        response=json.loads(path.with_suffix('.part').read_text())
        path.with_suffix('.part').replace(path)
        return response

    def inputs(**values):
        return dict(inputs=[dict(id=k,type='text/plain',value=str(v)) for k,v in values.items()])

    endpoint='https://api.water.usgs.gov/nldi/pygeoapi/processes/'
    for label in ['physical','indexed']:
        fetch(endpoint+'nldi-flowtrace/execution?f=json',cache/f'{label}_flowtrace.json',
            inputs(lon=record[f'{label}_longitude'],lat=record[f'{label}_latitude'],direction='none'))
    raw=json.loads((cache/'physical_flowtrace.json').read_text())
    feature=next(f for f in raw['features'] if f.get('id')=='nhdFlowline')
    if str(feature['properties'].get('comid'))!='23963685':
        raise RuntimeError('Official flowtrace changed the named Olney reach')
    longitude,latitude=feature['properties']['intersection_point']
    fetch(endpoint+'nldi-splitcatchment/execution?f=json',cache/'flowtraced_split_basin.json',
        inputs(lon=longitude,lat=latitude,upstream='true',simplified='false'))
    fetch('https://api.water.usgs.gov/nldi/linked-data/comid/23963685/navigation/UT/flowlines?f=json&distance=1000',
        cache/'upstream_flowlines.json')
    if not arguments.prepare_only:
        subprocess.run([rscript,'-'],input=source+'\nbuild_phase3_olney_barriers()\n',text=True,check=True)
    print('Official Olney flowtrace/basin/navigation caches prepared',flush=True)


if __name__=='__main__':
    os.chdir(Path(__file__).resolve().parents[1])
    main()
