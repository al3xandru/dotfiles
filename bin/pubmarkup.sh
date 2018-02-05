#!/bin/sh

SCRIPT="$WORKSPACES/mine/python/markdown/mkdn/markuptools.py"
NOTIFIER_APP=""
NOTIFIER_TYPE="0"
if [[ -x "/Applications/terminal-notifier.app/Contents/MacOS/terminal-notifier" ]]; then
    NOTIFIER_TYPE="1"
    NOTIFIER_APP="/Applications/terminal-notifier.app/Contents/MacOS/terminal-notifier"
elif [[ -x "/usr/local/bin/MountainNotifier" ]]; then
    NOTIFIER_TYPE="2"
    NOTIFIER_APP="/usr/local/bin/MountainNotifier"
fi
APP_NAME="com.metaclassy.byword"
if [[ "$1" = "-a" ]]; then
    shift
    if [[ $NOTIFIER_TYPE = "1" ]]; then
        APP_NAME=$1
    elif [[ $NOTIFIER_TYPE = "2" ]]; then
        APP_NAME=$(grep 'CFBundleIdentifier' -A1 "/Applications/$1.app/Contents/Info.plist" | sed s/string//g | sed s/key//g | sed s/[\<\>\/]//g | tail -1 | tr -d " \t") 
    fi
    shift
fi
if [[ "$1" = "--env" ]]; then
    OUTPUT=$(echo "$KMVAR_TextSel" | python "$SCRIPT" --type=m1 --tumblr -)
elif [ "$1" != "" ]; then
    OUTPUT=$(python "$SCRIPT" --type=m1 --tumblr "$1")
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
#echo "APP_NAME : $APP_NAME"
#echo "pub_state: $pub_state"
#echo "notif_msg: $notif_msg"
#echo "url      : $purl"
if [ $NOTIFIER_TYPE = "1" ]; then
    $NOTIFIER_APP -title "$APP_NAME:Markup" -subtitle "State: $pub_state" -message "$notif_msg" -open "$purl"
elif [ $NOTIFIER_TYPE = "2" ]; then
    $NOTIFIER_APP $APP_NAME "Markup" "State: $pub_state" "$notif_msg"
fi
