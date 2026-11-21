import sys
import os
from pathlib import Path
sys.path.insert(0,str(Path('spatial_data/barrier_reports/python_deps')))
sys.path.insert(0,str(Path(os.environ['TEMP'])/'habitat_pdf_parser'))
from pypdf import PdfReader
pdf=PdfReader('spatial_data/phase2/SFT_PAD_FINAL.pdf')
found=[]
for i,p in enumerate(pdf.pages):
    t=p.extract_text() or ''
    if any(s.lower() in t.lower() for s in ['SFT Falls','barrier falls','latitude','longitude']):
        found.append(f'PDF PAGE {i+1}\n'+t)
Path('spatial_data/phase2/tolt_source_extract.txt').write_text('\n\n'.join(found),encoding='utf-8')
print('Found pages:',[s.splitlines()[0] for s in found])
