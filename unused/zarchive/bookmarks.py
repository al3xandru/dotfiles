#!/usr/bin/python
#
# $Id: export_bookmarks 85 2004-09-29 18:07:32Z jay $
# Original script: http://www.macosxhints.com/dlfiles/export_bookmarks.txt
# Original Hint: http://www.macosxhints.com/article.php?story=2004092916004989
#
# Revised (probably poorly) to handle binary plist files via
# parsePlist handler function
#

import sys, os, codecs, plistlib

def read_bookmarks():
  bookmarks_file = os.path.join(os.getenv("HOME"), "Library/Safari/Bookmarks.plist")
  return parsePlist(bookmarks_file)

def parsePlist(bookmarks_file):
  try:
    return plistlib.Plist.fromFile(bookmarks_file)
  except:
    os.system("/usr/bin/plutil -convert xml1 %s" % bookmarks_file )
    xmlContent = plistlib.Plist.fromFile(bookmarks_file)
    os.system("/usr/bin/plutil -convert binary1 %s" % bookmarks_file )
    return xmlContent

def filter_bookmarks(node, wanted, found = 0, depth = 0):
  node_type  = node.get("WebBookmarkType")
  node_title = node.get("Title")
  if node_title == "BookmarksBar": 
    node_title ="Bookmarks Bar"
  indent = "t" * depth
  if node_type == "WebBookmarkTypeList":
    found = found or (node_title in wanted)
    if found and node_title:
      print "%s<DT><H3 FOLDED>%s</H3>" % (indent, node_title)
      print "%s<DL><p>" % indent
    for child in node.get("Children", []):
      filter_bookmarks(child, wanted, found, found*(depth+1)) 
    if found and node_title:
      print "%s</DL><p>" % indent
  elif node_type == "WebBookmarkTypeLeaf":
    if found:
      print '%s<DT><A HREF="%s" LAST_VISIT="%s">%s</A>' % (
        indent,
        node["URLString"],
        int(float(node["URIDictionary"].get("lastVisitedDate", 0))),
        node["URIDictionary"]["title"]
      )

def header():
  print """<!DOCTYPE NETSCAPE-Bookmark-file-1>
<HTML>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<Title>Bookmarks</Title>
<H1>Bookmarks</H1>"""

def footer():
  print """</HTML>"""

def main(args):
  sys.stdout = codecs.getwriter("utf-8")(sys.__stdout__)
  header()
  if args: 
    filter_bookmarks(read_bookmarks(), args)
  else: 
    filter_bookmarks(read_bookmarks(), [], found=1)
  footer()

if __name__ == "__main__":
  main(sys.argv[1:])   
