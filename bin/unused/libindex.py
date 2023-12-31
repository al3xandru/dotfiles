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
    _ignore = set()
    for root, dirs, files in os.walk(dir):
        if treat_as_file(root) or root in _ignore:
            for d in dirs:
                _ignore.add(os.path.join(root, d))
            continue
        _display = False
        for f in [f for f in dirs if treat_as_file(f)]:
            if not _display:
                fout.write(os.linesep)
                fout.write('[' + os.path.abspath(root) + ']')
                fout.write(os.linesep)
                _display = True
            fout.write(f)
            fout.write(os.linesep)
        for f in [f for f in files if accept(f, allowed_extensions, excluded_extensions)]:
            if not _display:
                fout.write(os.linesep)
                fout.write('[' + os.path.abspath(root) + ']')
                fout.write(os.linesep)
                _display = True
            fout.write(f)
            fout.write(os.linesep)


def treat_as_file(file):
    _, ext = os.path.splitext(file)
    if not ext:
        return False

    return ext.lower() in ('.app', '.key', '.kth', '.oo3', '..popclipext', '.1pif')
    

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


def display_shortcuts():
    print "Recipes:\n"
    print "Archives:"
    # print "-t all -i /
    print "Datastax:"
    print "-t all -i ~/Documents/MyDocs/50-projects/datastax/ -i ~/Documents/MyDocs/60-job/datastax/ -i /Volumes/archives/alx/DataStax/ -o ~/Documents/MyDocs/index/datastax.index.txt"
    print ""
    print "Docs:"
    print "-t docs -i ~/Dropbox/Magic\ Briefcase/Papers/ -i ~/Dropbox/Magic\ Briefcase/OtherPapers/ -i /Volumes/docs/ -o ~/Documents/MyDocs/index/docs.index.txt"

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-v', action='store_true', help='Display shortcuts')
    parser.add_argument('-t', '--type', action='store', 
                        help="Supported options: %s" % TYPES.keys())
    parser.add_argument('-i', '--input', action='append')
    parser.add_argument('-x', '--exclude', action='append')
    parser.add_argument('-o', '--output', nargs='?')
    
    opts = parser.parse_args()
    if not opts.input:
        opts.input = [os.path.abspath('.')]
    
    if opts.v:
        display_shortcuts()
    else:
        main(opts)
