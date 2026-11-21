import sys,os
from pathlib import Path
sys.path.insert(0,str(Path(os.environ['TEMP'])/'habitat_pdf_parser'))
from pypdf import PdfReader
p=PdfReader('spatial_data/phase2/SFT_PAD_FINAL.pdf')
for page_num in [130,239,240,339,340]:
    page=p.pages[page_num-1]
    print('PAGE',page_num,(page.extract_text() or '')[:2500])
    for i,image in enumerate(page.images):
        path=Path(f'spatial_data/phase2/tolt_page{page_num}_{i}_{image.name}')
        path.write_bytes(image.data)
        print(path)
