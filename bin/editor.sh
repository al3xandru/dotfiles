#!/bin/bash
# http://brettterpstra.com/2013/03/30/a-multi-purpose-editor-variable/
case "$1" in
    *_EDITMSG|*MERGE_MSG|*_TAGMSG )
        /usr/local/bin/vim "$1"
        ;;
    *crontab.* )
        /usr/local/bin/vim "$1"
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
