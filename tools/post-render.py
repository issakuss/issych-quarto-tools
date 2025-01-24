from pathlib import Path
import shutil

for ext in ['bst', 'cls', 'spl']:
    for path in Path().glob(f'*.{ext}'):
        path.rename(f'output/{path.name}')

if Path('output/output').exists():
    shutil.rmtree('output/output')
