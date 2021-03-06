#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# vim: ts=4 shiftwidth=4:
from __future__ import print_function

import argparse
import codecs
import os
import pipes
import subprocess
# import sys
import urllib

from xml.etree import ElementTree as ET


DEBUG = True if os.getenv('DEBUG_SCRIPT', False) == 'True' else False
FILE_PREFIX = 'qq-'
FILE_EXT = ('.md', '.markdown')
SAVE_DIR = os.path.expanduser('~/Dropbox/Dox/nvall')
SEARCH_DIRS = (SAVE_DIR,)


def main(action, options):
    if action == 'add':
        add(options.query)
    else:
        results = search(options)
        output_results(options.output, results, options.query)


def add(args):
    new_file = os.path.join(SAVE_DIR, "%s.md" % ' '.join(args))
    if os.path.exists(new_file):
        if DEBUG:
            print("WARN: A file for qq already exists '%s'" % new_file)
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


def search(options):
    cmd = ['mdfind']
    for dir in SEARCH_DIRS:
        cmd.append('-onlyin')
        cmd.append(pipes.quote(dir))

    if options.mode == 'interpret':
        cmd.append('-interpret')
        cmd.append(create_interpret_query(options))
    elif options.mode == 'literal':
        cmd.append('-literal')
        cmd.append(create_literal_query(options))
    else:
        print("[ERROR] Unknow mode:", options.mode)

    if DEBUG:
        print("[DEBUG]: %s" % cmd)
    results = subprocess.check_output(cmd)

    return results


def create_literal_query(options):
    query = []
    if options.prefix:
        query.append("kMDItemFSName=%s*" % options.prefix)

    attr = ""
    if options.text:
        attr = "kMDItemTextContent"
    else:
        attr = "kMDItemFSName"

    for t in options.query:
        query.append('&&')
        query.append("%s='*%s*'c" % (attr, t))

    query.append('&&')
    if len(FILE_EXT) > 1:
        query.append("(kMDItemFSName=*%s" % FILE_EXT[0])
        for ext in FILE_EXT[1:-1]:
            query.append("||")
            query.append("kMDItemFSName=*%s" % ext)
        query.append("|| kMDItemFSName=*%s)" % FILE_EXT[-1])
    else:
        query.append("kMDItemFSName=*%s" % FILE_EXT[0])

    return ' '.join(query)


def create_interpret_query(options):
    query = []
    if options.prefix:
        query.append("name:%s" % options.prefix)
        query.append('AND')
    if len(FILE_EXT) > 1:
        query.append("(name:%s" % FILE_EXT[0])
        for ext in FILE_EXT[1:-1]:
            query.append('OR')
            query.append("name:%s" % ext)
        query.append("OR name:%s)" % FILE_EXT[-1])
    else:
        query.append("name:%s" % FILE_EXT[0])
    if options.text:
        query.extend(options.query)
    else:
        query.extend(["AND name:%s" % t for t in options.query])

    return ' '.join(query)

def output_results(outformat, results, query):
    if outformat == 'text':
        output_as_text(results, query)
    elif outformat == 'alfred':
        output_as_alfred_xml(results, query)
    elif outformat == 'launchbar':
        output_as_launchbar_xml(results, query)


def output_as_text(results, query):
    if not results:
        print("Tough question... nothing :-(")
        return

    i = 1
    max_lines = 5
    for line in results.splitlines(False):
        bname = os.path.basename(line)
        bname = bname[:bname.rindex('.')]
        question = bname[len(FILE_PREFIX):].replace('_', ' ').replace('-', ' ').title()
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
    if DEBUG:
        print("[DEBUG] results:", (i-1))


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
        question = bname[len(FILE_PREFIX):].replace('_', ' ').replace('-', ' ')

        item = ET.SubElement(root, 'item')
        ET.SubElement(item, 'title').text = question.title() + '?'
        ET.SubElement(item, 'url').text = "nvalt://find/%s" % urllib.quote(bname)
        # ET.SubElement(item, 'path').text = line
        ET.SubElement(item, 'icon').text = u'qq.png'
        with codecs.open(line, 'r', 'utf8') as fin:
            for li in fin:
                if li.strip():
                    ET.SubElement(item, 'subtitle').text = li.strip()
                    break

        i += 1
    print(ET.tostring(root).encode('utf8'))


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
        question = bname[len(FILE_PREFIX):].replace('_', ' ').replace('-', ' ')
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
    print(ET.tostring(root).encode('utf8'))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Quick Question')
    parser.add_argument('-a', '--add', action='store_true')
    parser.add_argument('-o', '--output', action='store', choices=['launchbar', 'alfred', 'text'], default='text')
    parser.add_argument('--text', action='store_true')
    parser.add_argument('--prefix', action='store', default='qq-')
    parser.add_argument('--mode', action='store', choices=['interpret', 'literal'], default='interpret')
    parser.add_argument('query', nargs='*')
    options = parser.parse_args()

    action = 'add' if options.add else 'search'

    main(action, options)
