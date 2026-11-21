import sys
from pathlib import Path
sys.path.insert(0,str(Path('spatial_data/phase2/python_deps').resolve()))
import pymupdf
p=pymupdf.open('spatial_data/phase2/SFT_PAD_FINAL.pdf')
for n in [239,240,339,340]:
    print(n,p[n-1].get_text()[:100])
    p[n-1].get_pixmap(matrix=pymupdf.Matrix(1.5,1.5)).save(f'spatial_data/phase2/tolt_page{n}.png')
for n in [239,340]:
    print('PAGE OBJECT',n,p.xref_object(p[n-1].xref))
