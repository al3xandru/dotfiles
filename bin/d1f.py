#!/usr/bin/env python

import argparse
import codecs
import os
import subprocess
import sys
import urllib

from xml.etree import ElementTree as ET

DEBUG = False
FILE_EXT = '.doentry'
SEARCH_DIRS = (os.path.expanduser('~/Library/Mobile Documents/5U8NS4GX82~com~dayoneapp~dayone/Documents/Journal_dayone/'), )
# SEARCH_DIRS = (os.path.expanduser('~/Dropbox/Apps/Day One/Journal_dayone/'), )

def main(query, output='text'):
    results = search(query)
    output_results(output, results, query)

def search(query):
    cmd = ['mdfind']
    for dir in SEARCH_DIRS:
        cmd.append('-onlyin')
        cmd.append(dir)
    search_str = "( "
    search_str += 'kMDItemTextContent == "*%s*"wc' % query
    search_str += " )"
    cmd.append(search_str)
    if DEBUG:
        print "[DEBUG]: %s" % cmd
    results = subprocess.check_output(cmd)
    if DEBUG:
        for r in results.splitlines():
            print "[DEBUG]: %s" % r
    return results

def get_link(filename):
    bname = os.path.basename(filename)[:-len(FILE_EXT)]
    url = "dayone://edit?entryId=%s" % bname
    title = ""

    if os.path.isdir(filename):
        return bname, 'n/a', 'dir'

    _, ext = os.path.splitext(filename)
    if ext == '.jpg':
        title = os.path.basename(filename)
    else:
        with codecs.open(filename, 'r', 'utf8') as fin:
            title_line_next = False
            for li in fin:
                if not title_line_next and (li.strip() and li.lower().find('<key>entry text</key>') != -1):
                    title_line_next = True
                elif title_line_next:
                    _ltitle = li.strip().replace('<string>', '').replace('</string>', '')
                    if not _ltitle.startswith('<![CDATA['):
                        title = _ltitle.strip()
                        break
    if not title:
        title = u'Picture only (?)'
    elif title.strip().startswith('#'):
        title = title[title.find(' ')+1:]


    return bname, url, title

def output_results(output, results, query):
    """Support different output formats:

    - Alfred (custom XML)
    - LaunchBar (custom XML)
    - Markdown
    - text
    """
    if output == 'alfred':
        output_as_alfred_xml(results, query)
    elif output == 'launchbar':
        output_as_launchbar_xml(results, query)
    elif output in ('md', 'markdown'):
        output_as_markdown(results, query)
    else:
        output_as_text(results, query)

def output_as_text(results, query):
    for line in results.splitlines(False):
        _, url, title = get_link(line)
        print "%s \"%s\"" % (url, title)

def output_as_markdown(results, query):
    for line in results.splitlines(False):
        _, url, title = get_link(line)
        if not title:
            print ("<%s>" % url).encode('utf8')
        else:
            print ("[%s](%s)" % (title, url)).encode('utf8')

def output_as_alfred_xml(results, query):
    if not results:
        url = "dayone://entries"
        root = ET.Element('items')
        item = ET.SubElement(root, 'item',
            {'uid': 'notfound', 'arg': url, 'valid': 'yes'})
        ET.SubElement(item, 'title').text = u'No entries found'
        ET.SubElement(item, 'subtitle').text = u'Check out all entries'
        print(ET.tostring(root).encode('utf8'))
        return

    root = ET.Element('items')

    for line in results.splitlines(False):
        fid, url, title = get_link(line)
        item = ET.SubElement(root, 'item',
            {'uid': fid, 'arg': url, 'valid': 'yes'})
        ET.SubElement(item, 'title').text = title
        ET.SubElement(item, 'subtitle').text = url  # os.path.basename(line)[:-len(FILE_EXT)]

    print ET.tostring(root).encode('utf8')

def output_as_launchbar_xml(results, query):
    if not results:
        root = ET.Element('items')
        item = ET.SubElement(root, 'item')
        ET.SubElement(item, 'title').text = u'No entries found'
        ET.SubElement(item, 'subtitle').text = u'Check out all entries'
        ET.SubElement(item, 'icon').text = u'dayone.png'
        ET.SubElement(item, 'url').text = "dayone://entries"
        print(ET.tostring(root).encode('utf8'))
        return

    root = ET.Element('items')

    for line in results.splitlines(False):
        file_id, url, title = get_link(line)

        item = ET.SubElement(root, 'item')
        ET.SubElement(item, 'title').text = title
        ET.SubElement(item, 'subtitle').text = url
        ET.SubElement(item, 'url').text = url
        ET.SubElement(item, 'icon').text = u'dayone.png'

    print ET.tostring(root).encode('utf8')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='DayOne Finder')
    parser.add_argument('--debug', action='store_true', help='Enable debug details')
    parser.add_argument('-o', '--output', action='store', choices=['launchbar', 'alfred', 'md', 'markdown'],
        help='Specifies the output format')
    parser.add_argument('query', nargs='*')
    options = parser.parse_args()

    DEBUG = DEBUG or options.debug
    outputf = options.output or 'text'

    main(u' '.join(options.query), outputf)
