#!/usr/bin/env python
# encoding: utf-8
import argparse
import os
import sys


# When indexing the archive use the following exclusions:
# -x .jpg -x .png -x .psd -x .tiff -x .nib -x .plist -x .strings
ALL_EXTS = ('*',)
ARC_EXTS = ('.zip', '.tar', '.gz', '.rar', '.jar', '.tbz')
APP_EXTS = ('.app', '.dmg', '.pkg', '.jar')
DOC_EXTS= ('.doc', '.docs', '.epub', '.key', '.mobi', '.oo3', '.pdf', '.ppt', '.pptx')
PDF_EXTS = ('.pdf', '.ps')
PIC_EXTS = ('.bmp', '.gif', '.jpg', '.jpeg', '.png', '.svg')
TYPES = {
    'all': ALL_EXTS,
    'archive': ARC_EXTS,
    'archives': ARC_EXTS,
    'doc': DOC_EXTS,
    'docs': DOC_EXTS,
    'pdf': PDF_EXTS,
    'pdfs': PDF_EXTS,
    'pic': PIC_EXTS,
    'pics': PIC_EXTS,
    'image': PIC_EXTS,
    'images': PIC_EXTS
    }

def main(opts):
    """
    Just the main function


    :outfile: TODO
    :returns: TODO

    """
    allowed_extensions = TYPES[opts.type]
    excluded_extensions = opts.exclude or set('.ds_store')

    if opts.output:
        fout = open(opts.output, 'wb')
    else:
        fout = sys.stdout

    with fout:
        for indir in opts.input:
            process(indir, allowed_extensions, excluded_extensions, fout)
            fout.flush()


def process(dir, allowed_extensions, excluded_extensions, fout):
    """
    Scan a directory to index it.

    :dir: TODO
    :fout: TODO
    :returns: TODO

    """
    for root, dirs, files in os.walk(dir):
        _display = False
        for f in [f for f in files if accept(f, allowed_extensions, excluded_extensions)]:
            if not _display:
                fout.write(os.linesep)
                fout.write('[' + os.path.abspath(root) + ']')
                fout.write(os.linesep)
                _display = True
            fout.write(f)
            fout.write(os.linesep)


def accept(file, allowed_extensions, excluded_extensions):
    """
    Tests if the file should or should not be listed

    :file: TODO
    :returns: TODO

    """
    _, ext = os.path.splitext(file)
    ext = ext.lower()

    if (allowed_extensions[0] == '*') and (ext not in excluded_extensions):
        return True


    if (ext in allowed_extensions) and (ext not in excluded_extensions):
        return True

    return False


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-t', '--type', action='store', required=True)
    parser.add_argument('-i', '--input', action='append')
    parser.add_argument('-x', '--exclude', action='append')
    parser.add_argument('-o', '--output', nargs='?')
    
    opts = parser.parse_args()
    print opts
    if not opts.input:
        opts.input = [os.path.abspath('.')]
    
    main(opts)
