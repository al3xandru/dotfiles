#!/bin/bash
# http://brettterpstra.com/2013/03/30/a-multi-purpose-editor-variable/
if [ -x "/usr/local/bin/vim" ]; then
    VIM_CMD="/usr/local/bin/vim"
else
    VIM_CMD="/usr/bin/vim"
fi

# currently not used
MVIM_CMD=$(which mvim)
if [ -z "$MVIM_CMD" ]; then
	MVIM_CMD="$VIM_CMD"
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

if [ -z "$BBEDIT_CMD" ]; then
    TXT_ED_CMD="$VIM_CMD"
else
    TXT_ED_CMD="$BBEDIT_CMD"
fi

SUBL_CMD=$(which subl)
if [ -z "$SUBL_CMD" ]; then
	ED_CMD="$TXT_ED_CMD"
else
	ED_CMD="$SUBL_CMD"
fi

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
    *.txt|*.log )
        ${TXT_ED_CMD} "$1"
        ;;
    * )
        "$ED_CMD" "$1"
        ;;
esac
