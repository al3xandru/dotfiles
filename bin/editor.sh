#!/bin/bash
# http://brettterpstra.com/2013/03/30/a-multi-purpose-editor-variable/
if [ -f /usr/local/bin/vim ]; then
    VIM_CMD="/usr/local/bin/vim"
else
    VIM_CMD="/usr/bin/vim"
fi
case "$1" in
    *_EDITMSG|*MERGE_MSG|*_TAGMSG )
        $VIM_CMD "$1"
        ;;
    *crontab.* )
        $VIM_CMD "$1"
        ;;
    *.md|*.markdown|*.mdown|*.mkdown )
        /usr/local/bin/mmdc "$1"
        ;;
    *.txt )
        /usr/local/bin/bbedit "$1"
        ;;
    * )
        /usr/local/bin/bbedit -w "$1"
        ;;
esac
