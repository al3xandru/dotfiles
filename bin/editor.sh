#!/bin/bash
# http://brettterpstra.com/2013/03/30/a-multi-purpose-editor-variable/
if [ -x "/usr/local/bin/vim" ]; then
    VIM_CMD="/usr/local/bin/vim"
else
    VIM_CMD="/usr/bin/vim"
fi
if [ -x "/usr/local/bin/mmdc" ]; then
    MARKDOWN_CMD="/usr/local/bin/mmdc"
else
    MARKDOWN_CMD="$VIM_CMD"
fi
if [ -x "/usr/local/bin/bbedit" ]; then
    BBEDIT_CMD="/usr/local/bin/bbedit"
else
    BBEDIT_CMD="$VIM_CMD"
fi
case "$1" in
    *_EDITMSG|*MERGE_MSG|*_TAGMSG )
        $VIM_CMD "$1"
        ;;
    *crontab.* )
        $VIM_CMD "$1"
        ;;
    *.md|*.markdown|*.mdown|*.mkdown )
        $MARKDOWN_CMD "$1"
        ;;
    *.txt )
        $BBEDIT_CMD "$1"
        ;;
    * )
        $BBEDIT_CMD -w "$1"
        ;;
esac
