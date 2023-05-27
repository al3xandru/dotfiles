#!/usr/bin/env bash

function syncFiles() {
    echo ""
}

function syncDirs() {
    syncLibraryApplicationSupport
    syncLibraryPreferences
    syncLibraryScripts
    # syncLibraryServices
}

function syncLibraryApplicationSupport() {
    SRC_DIR="$HOME/Library/Application Support/"
    TO_DIR="$TARGET_DIR/Application Support/$MACHINE_NAME"

    printf "... start sync for '%s' to '%s'\n" "$SRC_DIR" "$TO_DIR"
    printf "...... backup in '%s/zbackups/%s'\n\n" "$TO_DIR" "$(date +%Y%m%d)"

    mkdir -p "$TO_DIR/zbackups/$(date +%Y%m%d)"

    rsync -rlptgomEb --backup-dir="zbackups/$(date +%Y%m%d)" --delete --exclude=com.apple.* --exclude=zbackups --exclude=zbackup-* --exclude-from="$HOME/bin/sync_confs_rsync_exclude_lib_as.txt" --no-links "$SRC_DIR" "$TO_DIR"
    echo "$(date +"%Y-%m-%d %H:%M:%S")" >> "${TO_DIR}_timestamp.log"
}

function syncLibraryPreferences() {
    SRC_DIR="$HOME/Library/Preferences/"
    TO_DIR="$TARGET_DIR/Preferences/$MACHINE_NAME"

    printf "... start sync for '%s' to '%s'\n\n" "$SRC_DIR" "$TO_DIR"

    mkdir -p "$TO_DIR/zbackups/$(date +%Y%m%d)"

    rsync -rlptgomEb --no-links --backup-dir="zbackups/$(date +%Y%m%d)" --delete --exclude=com.apple.* --exclude=zbackups --exclude=com.dropbox.* --exclude=ContextStoreAgent.plist --exclude=knowledge-agent.plist --exclude=loginwindow.plist --exclude=mbuseragent.plist --exclude=MobileMeAccounts.plist --exclude=siriknowledged.plist "$SRC_DIR" "$TO_DIR"
    echo "$(date +"%Y-%m-%d %H:%M:%S")" >> "${TO_DIR}_timestamp.log"
}

function syncLibraryScripts() {
    SRC_DIR="$HOME/Library/Scripts/"
    TO_DIR="$TARGET_DIR/Scripts/$MACHINE_NAME"

    printf "... start sync for '%s' to '%s'\n\n" "$SRC_DIR" "$TO_DIR"

    mkdir -p "$TO_DIR/zbackups/$(date +%Y%m%d)"

    rsync -rlptgomEb --backup-dir="zbackups/$(date +%Y%m%d)" --delete --exclude=zbackups "$SRC_DIR" "$TO_DIR"
    echo "$(date +"%Y-%m-%d %H:%M:%S")" >> "${TO_DIR}_timestamp.log"
}

function syncLibraryServices() {
    SRC_DIR="$HOME/Library/Services/"
    TO_DIR="$TARGET_DIR/Services/$MACHINE_NAME"

    printf "... start sync for '%s' to '%s'\n\n" "$SRC_DIR" "$TO_DIR"

    mkdir -p "$TO_DIR"

    rsync -rlptgomEb --backup-dir="zbackup-$(date +%Y%m%d)" --delete --exclude=zbackup-* "$SRC_DIR" "$TO_DIR"
    echo "$(date +"%Y-%m-%d %H:%M:%S")" >> "${TO_DIR}_timestamp.log"
}


if [ -z "$TARGET_DIR" ]; then
    TARGET_DIR="${HOME}/Dropbox/ApplicationSupport/dirs/Library"
fi
MACHINE_NAME=`hostname -s`
if [ ! -d "$TARGET_DIR" ]; then
    printf "ERROR: Target directory does not exist $TARGET_DIR\n"
    exit 1
fi

printf "Synching config files for machine: ${MACHINE_NAME} to ${TARGET_DIR}\n\n"
syncDirs
