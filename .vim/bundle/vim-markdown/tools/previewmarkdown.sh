STYLESHEET='file:///Users/alex/.vim/markdownpreview_plugin/markdownprev2.css'
TITLE=$(basename "$1")
HTML_BODY=$(/Users/alex/bin/markup.sh "$1")

HTML=$(cat <<EOF
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<link rel="stylesheet" href="${STYLESHEET}"></link>
<title>${TITLE}</title>
</head>
<body>
  <div id="article">
    <div class="page"> 
    ${HTML_BODY}
    </div>
  </div>
</body>
</html>
EOF)

OUTPUT_HTML="$1.html"
#echo "Input : '$1'"
#echo "Title :$TITLE"
#echo "Output:$OUTPUT_HTML"
echo "${HTML}" > "$OUTPUT_HTML" 
open -a Safari "$OUTPUT_HTML"
sleep 3
rm "$OUTPUT_HTML"
