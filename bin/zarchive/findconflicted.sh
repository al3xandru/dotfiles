#!/bin/bash
DROPBOXDIR="$HOME/Dropbox"

function notify {
	# say "Conflicted files in Dropbox"
    echo
}

function find_conflicts {
	if [ `find "$DROPBOXDIR" \( -regex ".*/.* conflicted .*" -and \! -regex ".*/.dropbox.cache/.*" \) | wc -l ` -gt 0 ]; then 
		notify
	fi
}

function clean_conflicts {
	conflicted_files=`find "$DROPBOXDIR" \( -regex ".*/.* conflicted .*" -and \! -path "*/.dropbox.cache/*" \)`
	#echo -n "'$conflicted_files'"
	#count_files=`echo -n $conflicted_files | wc -l`
	if [ "$conflicted_files" != "" ]; then
	    echo "There are some conflicted files:"
	    echo "$conflicted_files"
	    read -p "Do you want to delete these conflict files? (y/n): "
	    if [ $REPLY = "y" ]; then
	        find "$DROPBOXDIR" \( -regex ".*/.* conflicted .*" -and \! -path "*/.dropbox.cache/*" \) -exec rm -vf {} \;
	    fi
	fi	
}

if [ "$1" == "-s" ]; then
	find_conflicts
else
	clean_conflicts
fi
