#!/bin/sh

SCRIPT="$HOME/Dropbox/workspace/python/markdown/mkdn/markuptools.py"
#SCRIPT="$HOME/tmp/testpubmarkup.sh"
APP_NAME="com.metaclassy.byword"
if [[ "$1" = "-a" ]]; then
    shift
    APP_NAME=$(grep 'CFBundleIdentifier' -A1 "/Applications/$1.app/Contents/Info.plist" | sed s/string//g | sed s/key//g | sed s/[\<\>\/]//g | tail -1 | tr -d " \t") 
    shift
fi
if [[ "$1" = "--env" ]]; then
    OUTPUT=$(echo "$KMVAR_TextSel" | python "$SCRIPT" --type=m1 --tumblr -)
else
    OUTPUT=$(python "$SCRIPT" --type=m1 --tumblr -)
fi
pub_pattern="publish: ([^${IFS}]*)"
tweet_pattern="tweet: ([^${IFS}]*)"
pid_pattern="post_id: ([^${IFS}]*)"
purl_pattern="post_uri: ([^${IFS}]*)"
pub_state=""
purl=""
notif_msg=""
if [[ $OUTPUT =~ $pub_pattern ]]; then
    pub_state="${BASH_REMATCH[1]}"
    #echo "pub_state: ${BASH_REMATCH[1]} -> $pub_state"
    say "$pub_state"
fi
if [[ $OUTPUT =~ $tweet_pattern ]]; then
    notif_msg="$notif_msg${IFS}Tweet: ${BASH_REMATCH[1]}"
fi
if [[ $OUTPUT =~ $pid_pattern ]]; then
    #echo "id: ${BASH_REMATCH[1]}"
    notif_msg="$notif_msg${IFS}Id: ${BASH_REMATCH[1]}"
fi
if [[ $OUTPUT =~ $purl_pattern ]]; then
    purl="${BASH_REMATCH[1]}"
    #echo "url: $purl"
    echo "$purl" | pbcopy
    notif_msg="$notif_msg${IFS}Url: $purl"
fi
#echo "APP_NAME: $APP_NAME"
#echo "pub_state: $pub_state"
#echo "notif_msg: $notif_msg"
/usr/local/bin/MountainNotifier $APP_NAME "Markup" "State: $pub_state" "$notif_msg"
