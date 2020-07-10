# Rename columns

The purpose of this program is to rename the columns in one or more input files according to a "mapping" file which should have the following structure:

```bash
$ csvchk mapping.csv
// ****** Record 1 ****** //
Column.Name   : Date
Template.Name : yearCollected
```

That is, any column found in "Column.Name" should be renamed to the value in "Template.Name."

The `colrname.py` program take the following arguments:

```bash
$ ./colrename.py -h
usage: colrename.py [-h] -m FILE [-f FILE [FILE ...]] [-o DIR]

Rename columns in CSV file

optional arguments:
  -h, --help            show this help message and exit
  -m FILE, --mapping_file FILE
                        Column mapping file (default: None)
  -f FILE [FILE ...], --file FILE [FILE ...]
                        Input file(s) to fix (default: None)
  -o DIR, --outdir DIR  Output directory (default: out)
```

Each `--file` argument will be processed using the `--mapping_file` values and will be written into the `--outdir` directory (which will be created if needed).
For example, the "cougar.csv" file has the following fields:

```bash
$ csvchk cougar.csv
// ****** Record 1 ****** //
Date            : 5/19/87
Management Unit : Mt Emily
County          : Umatilla
Sex             : F
Age             : 4
Status          : A
Weight          : 105
Length          : 75
```

As shown in the `Makefile`, you can execute the program with the provided data like so:

```bash
$ ./colrename.py -m mapping.csv -f cougar.csv
  1: Processing "cougar.csv
Done.
```

There will now be an "out/cougar.csv" file you can inspect:

```bash
$ csvchk out/cougar.csv
// ****** Record 1 ****** //
yearCollected   : 5/19/87
Management Unit : Mt Emily
County          : Umatilla
Sex             : F
Age             : 4
Status          : A
Weight          : 105
Length          : 75
```

## Author

Ken Youens-Clark <kyclark@arizona.edu>
