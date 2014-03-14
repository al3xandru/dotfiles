#!/usr/local/env python

import argparse
import codecs
import os
import subprocess
import sys
import urllib

from xml.etree import ElementTree as ET

DEBUG = False
FILE_PREFIX = 'qq-'
FILE_EXT = '.md'
SAVE_DIR = os.path.expanduser('~/Dropbox/ka/nvall')
SEARCH_DIRS = (SAVE_DIR, os.path.expanduser('~/Dropbox/ka/knarc/Files'))

def main(action, args):
	if action == 'add':
		add(args)
	else:
		search(args)

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
	cmd.append("filename:%s AND filename:%s AND %s" % (FILE_PREFIX, FILE_EXT, ' '.join(args)))
	if DEBUG:
		print "[DEBUG]: %s" % cmd
	results = subprocess.check_output(cmd)
	if results:
		output_results(results)
	else:
		url = "nvalt://find/qq-%s" % urllib.quote(' '.join(args))
		root = ET.Element('items')
		item = ET.SubElement(root, 'item',
			{'uid': 'notfound', 'arg': url, 'valid': 'yes'})
		ET.SubElement(item, 'title').text = u'Tough question...'
		ET.SubElement(item, 'subtitle').text = u'Create an answer'
		ET.SubElement(item, 'icon').text = u'qq.png'
		print ET.tostring(root).encode('utf8')

def output_results(result_output):
	root = ET.Element('items')

	xml_result = u""
	i = 0
	for line in result_output.splitlines(False):
		bname = os.path.basename(line)[:-len(FILE_EXT)]
		question = bname[len(FILE_PREFIX):]
		url = "nvalt://find/%s" % urllib.quote(bname)
		item = ET.SubElement(root, 'item', 
			{'uid': question.replace(' ', ''), 'arg': url, 'valid': 'yes'})
		ET.SubElement(item, 'title').text = question + '?'
		with codecs.open(line, 'r', 'utf8') as fin:
			for li in fin:
				if li.strip():
					ET.SubElement(item, 'subtitle').text = li.strip()
					break
		ET.SubElement(item, 'icon').text = u'qq.png'
		i += 1
	print ET.tostring(root).encode('utf8')

if __name__ == '__main__':
	if len(sys.argv) < 2:
		print "ERROR: Usage %s [-a|--add] question" % sys.argv[0]
		sys.exit(1)
	action = 'search'
	idx = 1
	if sys.argv[1] in ('-a', '--add'):
		action = 'add'
		idx = 2
	main(action, sys.argv[idx:])
