#!/usr/bin/env python

import argparse
import codecs
import os
import subprocess
# import sys
import urllib

from xml.etree import ElementTree as ET


DEBUG = False
FILE_PREFIX = 'qq-'
FILE_EXT = ('.md', '.markdown')
SAVE_DIR = os.path.expanduser('~/Dropbox/Dox/nvall')
# SEARCH_DIRS = (SAVE_DIR, os.path.expanduser('~/Dropbox/ka/knarc/Files'))
SEARCH_DIRS = (SAVE_DIR,)


def main(action, args, output='text'):
    if action == 'add':
        add(args)
    else:
        results = search(args)
        output_results(output, results, args)


def add(args):
    new_file = os.path.join(SAVE_DIR, "%s.md" % ' '.join(args))
    if os.path.exists(new_file):
        if DEBUG:
            print "WARN: A file for qq already exists '%s'" % new_file
    else:
        with open(new_file, 'a+') as fout:
            fout.flush()
    subprocess.call(['open', new_file])

_NO_RESULTS = u"""
<?xml version="1.0"?>
<items>
  <item uid="noresults" arg="noresults" valid="yes">
    <title>No results</title>
    <subtitle>Unfortunately your quick question remained unanswered</subtitle>
  </item>
</items>
"""


def search(args):
    cmd = ['mdfind']
    for dir in SEARCH_DIRS:
        cmd.append('-onlyin')
        cmd.append(dir)
    cmd.append('-interpret')
    cmd.append(create_query(args))
    if DEBUG:
        print "[DEBUG]: %s" % cmd
    results = subprocess.check_output(cmd)

    return results


def create_query(args):
    query = ("filename:%s" % FILE_PREFIX)
    if len(FILE_EXT) > 1:
        query += ' AND ('
        query += ' OR '.join(["filename:%s" % t for t in FILE_EXT])
        query += ')'
    else:
        query += " AND filename:%s" % FILE_EXT[0]
    query += " AND %s" % ' '.join(args)

    return query

def output_results(outformat, results, query):
    if outformat == 'text':
        output_as_text(results, query)
    elif outformat == 'alfred':
        output_as_alfred_xml(results, query)
    elif outformat == 'launchbar':
        output_as_launchbar_xml(results, query)


def output_as_text(results, query):
    if not results:
        print "Tough question... nothing :-("
        return

    i = 1
    max_lines = 5
    for line in results.splitlines(False):
        bname = os.path.basename(line)
        bname = bname[:bname.rindex('.')]
        question = bname[len(FILE_PREFIX):].title()
        answer = u""
        with codecs.open(line, 'r', 'utf8') as fin:
            count_lines = 0
            for li in fin:
                if li.strip():
                    count_lines += 1
                    answer = answer + li.strip() + "\n"
                if count_lines >= max_lines:
                    answer = answer[:-1] + ' [...]'
                    break
        print("%d. [\"%s\"]\nQ: %s?\nA: %s\n" % (i, line, question, answer))
        i += 1
    # print "results:", i


def output_as_launchbar_xml(results, query):
    if not results:
        root = ET.Element('items')
        item = ET.SubElement(root, 'item')
        ET.SubElement(item, 'title').text = u'Tough question...'
        ET.SubElement(item, 'subtitle').text = u'Create an answer'
        ET.SubElement(item, 'icon').text = u'qq.png'
        ET.SubElement(item, 'url').text = "nvalt://find/qq-%s" % urllib.quote(' '.join(query))
        print(ET.tostring(root).encode('utf8'))
        return

    root = ET.Element('items')

    i = 0
    for line in results.splitlines(False):
        bname = os.path.basename(line)[:-len(FILE_EXT)]
        question = bname[len(FILE_PREFIX):]

        item = ET.SubElement(root, 'item')
        ET.SubElement(item, 'title').text = question + '?'
        ET.SubElement(item, 'url').text = "nvalt://find/%s" % urllib.quote(bname)
        # ET.SubElement(item, 'path').text = line
        ET.SubElement(item, 'icon').text = u'qq.png'
        with codecs.open(line, 'r', 'utf8') as fin:
            for li in fin:
                if li.strip():
                    ET.SubElement(item, 'subtitle').text = li.strip()
                    break

        i += 1
    print ET.tostring(root).encode('utf8')


def output_as_alfred_xml(results, query):
    if not results:
        url = "nvalt://find/qq-%s" % urllib.quote(' '.join(query))
        root = ET.Element('items')
        item = ET.SubElement(root, 'item',
                             {'uid': 'notfound', 'arg': url, 'valid': 'yes'})
        ET.SubElement(item, 'title').text = u'Tough question...'
        ET.SubElement(item, 'subtitle').text = u'Create an answer'
        ET.SubElement(item, 'icon').text = u'qq.png'
        print(ET.tostring(root).encode('utf8'))
        return

    root = ET.Element('items')

    # xml_result = u""
    i = 0
    for line in results.splitlines(False):
        bname = os.path.basename(line)
        bname = bname[:bname.rindex('.')]
        question = bname[len(FILE_PREFIX):]
        url = "nvalt://find/%s" % urllib.quote(bname)
        item = ET.SubElement(root, 'item',
                             {'uid': question.replace(' ', ''), 'arg': url, 'valid': 'yes'})
        ET.SubElement(item, 'title').text = question.title()+ '?'
        lineCount = 0
        hasSubtitle = False
        subtitle = u''
        with codecs.open(line, 'r', 'utf8') as fin:
            for li in fin:
                if li.strip():
                    lineCount += 1
                    if not subtitle:
                        subtitle = li.strip('# \n')
                        hasSubtitle = True

        ET.SubElement(item, 'subtitle').text = "%s [...] (%d lines)" % (subtitle, lineCount)
        ET.SubElement(item, 'icon').text = u'qq.png'
        i += 1
    print ET.tostring(root).encode('utf8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Quick Question')
    parser.add_argument('-a', '--add', action='store_true')
    parser.add_argument('-o', '--output', action='store', choices=['launchbar', 'alfred'])
    parser.add_argument('query', nargs='*')
    options = parser.parse_args()

    action = 'add' if options.add else 'search'
    outputf = options.output or 'text'

    main(action, options.query, outputf)
