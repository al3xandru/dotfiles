#!/bin/bash
bookmarks=$HOME/Library/Safari/Bookmarks.plist
plutil -convert xml1 $bookmarks
sed >/tmp/plist2html.xsl <<EOF
<xsl:stylesheet version='1.0'
xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
xmlns='http://www.w3.org/1999/xhtml'>

<xsl:output method='xml' indent='no'
doctype-public='-//W3C//DTD XHTML 1.0 Transitional//EN'
doctype-system='http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'
omit-xml-declaration='no'
media-type='application/xhtml+xml; charset=utf-8'/>

<xsl:template match='/'>
<html>
<head>
<title>Safari Bookmarks</title>
</head>
<body>
<h1>Safari Bookmarks</h1>
<ul>
<xsl:apply-templates select='plist/dict'/>
</ul>
</body>
</html>
</xsl:template>

<xsl:template metch='array'>
<ul>
<xsl:apply-templates/>
</ul>
</xsl:template>

<xsl:template match='dict'>
<li>
<xsl:choose>
<xsl:when test='string[text()="WebBookmarkTypeLeaf"]'>
<xsl:apply-templates select='key[text()="URIDictionary"]/following-sibling::dict[1]' mode='URIDictionary'/>
</xsl:when>
<xsl:when test='string[text()="WebBookmarkTypeList"]'>
<xsl:if test='key[text()="Title"]'>
<h3><xsl:apply-templates select='key[text()="Title"]/following-sibling::string[1]'/></h3>
</xsl:if>
</xsl:when>
</xsl:choose>
<xsl:apply-templates select='key[text()="Children"]/following-sibling::array[1]'/>
</li>
</xsl:template>

<xsl:template match='dict' mode='URIDictionary'>
<a href='{string[1]}'>
<xsl:value-of select='key[text()="title"]/following-sibling::string[1]'/>
</a>
</xsl:template>

</xsl:stylesheet>
EOF
xsltproc /tmp/plist2html.xsl $bookmarks >index.html
