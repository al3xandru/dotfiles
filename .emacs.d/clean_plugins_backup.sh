#!/bin/bash
if [ -z "$1" ]; then
    echo "$0 dir"
    exit
fi
outfile="/tmp/$(basename $1).txt"
ls -A1 "$1" | sort > $outfile
ls -A1 "$HOME/.emacs.d/elpa" | sort > /tmp/elpa.txt
diff $outfile /tmp/elpa.txt
echo ""
echo "Cleanup command:"
echo "find $1 -type d \! -name -delete"
