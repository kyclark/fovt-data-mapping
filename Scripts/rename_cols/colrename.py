#!/usr/bin/env python3
"""
Author : Ken Youens-Clark <kyclark@gmail.com>
Date   : 2020-07-10
Purpose: Rename columns in CSV file
"""

import argparse
import csv
import os
from typing import Dict, TextIO


# --------------------------------------------------
def get_args():
    """Get command-line arguments"""

    parser = argparse.ArgumentParser(
        description='Rename columns in CSV file',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('-m',
                        '--mapping_file',
                        help='Column mapping file',
                        metavar='FILE',
                        type=argparse.FileType('rt'),
                        required=True)

    parser.add_argument('-f',
                        '--file',
                        help='Input file(s) to fix',
                        metavar='FILE',
                        type=argparse.FileType('rt'),
                        nargs='+')

    parser.add_argument('-o',
                        '--outdir',
                        help='Output directory',
                        metavar='DIR',
                        type=str,
                        default='out')

    return parser.parse_args()


# --------------------------------------------------
def main() -> None:
    """Make a jazz noise here"""

    args = get_args()
    out_dir = args.outdir

    if not os.path.isdir(out_dir):
        os.makedirs(out_dir)

    mapping = read_mapping(args.mapping_file)

    for i, fh in enumerate(args.file, start=1):
        print(f'{i:3}: Processing "{os.path.basename(fh.name)}')
        process(fh, mapping, out_dir)

    print('Done.')


# --------------------------------------------------
def process(fh: TextIO, mapping: Dict[str, str], out_dir: str) -> bool:
    """Modify a file's headers, write to outdir"""

    reader = csv.DictReader(fh, delimiter=',')
    reader.fieldnames = list(
        map(lambda f: mapping.get(f, f), reader.fieldnames))

    out_file = os.path.join(out_dir, os.path.basename(fh.name))
    writer = csv.DictWriter(open(out_file, 'wt'),
                            delimiter=',',
                            fieldnames=reader.fieldnames)
    writer.writeheader()

    for rec in reader:
        writer.writerow(rec)

    return True


# --------------------------------------------------
def read_mapping(fh: TextIO) -> Dict[str, str]:
    """Read the mapping file into a dictionary"""

    mapping = {}
    reader = csv.DictReader(fh, delimiter=',')

    for rec in reader:
        col_name, tmpl_name = rec.get('Column.Name'), rec.get('Template.Name')
        if col_name and tmpl_name:
            mapping[col_name] = tmpl_name

    return mapping


# --------------------------------------------------
if __name__ == '__main__':
    main()
