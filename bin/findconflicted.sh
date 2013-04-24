#!/bin/bash
conflicted_files=`find ~/Dropbox \( -regex ".*/.* conflicted .*" -and \! -path "*/.dropbox.cache/*" \)`
#echo -n "'$conflicted_files'"
#count_files=`echo -n $conflicted_files | wc -l`
if [ "$conflicted_files" != "" ]; then
    echo "Conflicted files:"
    echo "$conflicted_files"
    read -p "Do you want to delete them? (y/n): "
    if [ $REPLY = "y" ]; then
        find ~/Dropbox \( -regex ".*/.* conflicted .*" -and \! -path "*/.dropbox.cache/*" \) -exec rm -vf {} \;
    fi
fi
