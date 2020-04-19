from subprocess import Popen, PIPE
from shutil import move
import glob
import os

def main():
    os.putenv('OCAMLRUNPARAM', 'b')
    if not os.path.exists('tests'): os.mkdir('tests')
    elif not os.path.exists('tests/old'):
        os.mkdir('tests/old')
    for fp in glob.glob('tests/*.result'):
        move(fp, f'tests/old/{fp[6:]}')

    codes = {0: 'SUCCESS', 1: 'FAILURE', 2: 'RAISED'}
    result = {v: [] for v in codes.values()}

    for fp in glob.glob('examples/*.ml'):
        fp = fp[:-3] # remove extension
        caml_file, lambda_file = f'{fp}.ml', f'{fp}.lambda'
        fp = fp[9:] # remove examples/
        if not os.path.exists(lambda_file):
            raise Exception(f'File {lambda_file} does not exist')


        p = Popen(['dune', 'exec', 'engine/main.bc', lambda_file, caml_file],
                  stdin=PIPE, stdout=PIPE, stderr=PIPE)
        stdout, stderr = p.communicate()
        rc = p.returncode

        result[codes[rc]].append(fp)
        with open(f'tests/{fp}.result', 'w') as f:
            f.write((stdout+stderr).decode())

    return result

        

if __name__ == '__main__':
    from pprint import pprint
    pprint(main())
