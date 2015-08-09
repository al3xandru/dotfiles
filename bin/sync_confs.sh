#!/bin/bash
declare -a CONF_FILES
CONF_FILES=("${HOME}"/.MacOSX-renamed/environment.donotuse
    "${HOME}/Library/Application Support/Karabiner/private.xml"
    "${HOME}/Library/Application Support/KeyRemap4MacBook/private.xml"
    "${HOME}"/"Library/Application Support/LaunchBar/Configuration.plist"
    "${HOME}"/"Library/Application Support/LaunchBar/CustomShortcuts.plist"
    "${HOME}"/"Library/Application Support/BetterTouchTool/bttdata2"
    "${HOME}"/Library/KeyBindings/DefaultKeyBinding.dict
    "${HOME}"/Library/Preferences/com.apple.Safari.plist
    "${HOME}"/Library/Colors/SkimNotes.clr
    "${HOME}"/Library/Colors/Solarized.clr
)

if [ -z "$TARGET_DIR" ]; then
    TARGET_DIR="${HOME}/Dropbox/ApplicationSupport/configfiles"
fi
MACHINE_NAME=`hostname -s`

echo "Synching config files for machine: ${MACHINE_NAME}"
echo "into: ${TARGET_DIR}"
echo ""

for fi in "${CONF_FILES[@]}"
    do
        must_copy="false"
        fname=`basename "$fi"`
        fdname=`dirname "$fi"`
        dname=`basename "$fdname"`
        tfile="${TARGET_DIR}"/"${MACHINE_NAME}-${dname}-${fname}"
        echo "Processing: '$fi'"
        echo "Target    : '$tfile'"
        if [ -e "$tfile" ]; then
            echo "Target file exists"
            if [ "$fi" -nt "$tfile" ]; then
                must_copy="true"
                echo "Target file is old"
            else
                if [ "$fi" -ot "$tfile" ]; then
                    echo "Target file is newer"
                    if [ -e "/usr/local/bin/growlnotify" ]; then
                        /usr/local/bin/growlnotify -n "Conf Sync" -s -m "Target file is newer ${tfile}"
                    else
                        echo "`date \"+%Y-%m-%d %H:%M:%S\"` target file is newer ${tfile}" >> ~/Library/Logs/com.mypopescu.confsync.log
                    fi
                fi
            fi
        else
            echo "Target file does not exist"
            must_copy="true"
        fi
        if [ "${must_copy}" = "true" ]; then
            echo "Copying..."
            cp -vfp "${fi}" "${tfile}"
        fi
        echo ""
    done
