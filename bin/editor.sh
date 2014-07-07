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
fi
if [ -x "/usr/local/bin/atom" ]; then
    ATOM_CMD="/usr/local/bin/atom"
else
    ATOM_CMD="$VIM_CMD"
fi
if [ -x "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl" ]; then
    SUBL_CMD="/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"
else
    if [ -x "/Applications/ST3.app/Contents/SharedSupport/bin/subl" ]; then
        SUBL_CMD="/Applications/ST3.app/Contents/SharedSupport/bin/subl"
    else
        if [ -x "/Applications/ST2.app/Contents/SharedSupport/bin/subl" ]; then
            SUBL_CMD="/Applications/ST2.app/Contents/SharedSupport/bin/subl"
        fi
    fi
fi
if [ -z "$BBEDIT_CMD" ]; then
    TXT_ED_CMD="$VIM_CMD"
else
    TXT_ED_CMD="$BBEDIT_CMD"
fi
ED_CMD="$SUBL_CMD"
case "$1" in
    *_EDITMSG|*MERGE_MSG|*_TAGMSG )
        ${VIM_CMD} "$1"
        ;;
    *crontab.* )
        ${VIM_CMD} "$1"
        ;;
    *.md|*.markdown|*.mdown|*.mkdown )
        ${MARKDOWN_CMD} "$1"
        ;;
    *.txt )
        ${TXT_ED_CMD} "$1"
        ;;
    * )
        "$ED_CMD" -w "$1"
        ;;
esac
