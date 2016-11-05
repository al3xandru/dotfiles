#!/bin/bash
# http://brettterpstra.com/2013/03/30/a-multi-purpose-editor-variable/
if [ -x "/usr/local/bin/vim" ]; then
    VIM_CMD="/usr/local/bin/vim"
else
    VIM_CMD="/usr/bin/vim"
fi


if [ -x "/usr/local/bin/mmdc" ]; then
    MARKDOWN_CMD="/usr/local/bin/mmdc"
else
    MARKDOWN_CMD="$VIM_CMD"
fi

if [ -x "/usr/local/bin/bbedit" ]; then
    BBEDIT_CMD="/usr/local/bin/bbedit"
else
    BBEDIT_CMD="$VIM_CMD"
fi

# currently not used
MVIM_CMD=$(which mvim)
if [ -z "$MVIM_CMD" ]; then
	MVIM_CMD="$VIM_CMD"
fi
# if [ -x "/usr/local/bin/atom" ]; then
#     ATOM_CMD="/usr/local/bin/atom"
# else
#     ATOM_CMD="$VIM_CMD"
# fi

# Getting the last argument
# (works with bash/ksh only): ${@: -1}
# (works with bash 3.x+ only): ${!#}
# (works with bash 3.x+ only): $BASH_ARGV
# (works with bash 3.x+ / ksh only): ${@:$#}
# (works with bash 3.x+ only): ${BASH_ARGV[0]}
# (portable version): 
# for i in $@; do :; done
# echo "$i"
lastarg="${BASH_ARGV[0]}"
case "$lastarg" in
    *_EDITMSG|*MERGE_MSG|*_TAGMSG )
        ${VIM_CMD} "$lastarg"
        ;;
    *crontab.* )
        ${VIM_CMD} "$lastarg"
        ;;
    *.md|*.markdown|*.mdown|*.mkdown )
        ${BBEDIT_CMD} "$lastarg"
        ;;
    *.txt|*.log )
        ${MVIM_CMD} "$lastarg"
        ;;
    * )
        ${MVIM_CMD} "$lastarg"
        ;;
esac
# if [ -z "$BBEDIT_CMD" ]; then
#     TXT_ED_CMD="$VIM_CMD"
#     ED_CMD="$VIM_CMD"
# else
#     TXT_ED_CMD="$BBEDIT_CMD"
#     ED_CMD="$BBEDIT_CMD"
# fi

# SUBL_CMD=$(which subl)
# if [ -z "$SUBL_CMD" ]; then
# 	ED_CMD="$TXT_ED_CMD"
# else
# 	ED_CMD="$SUBL_CMD"
# fi

# Can treat directories differently
# 
# if [ -d "$1" ]; then
#     _cdir=$(pwd)
#     _ddir=$(cd "$1"; pwd)
#     _cmd="tell application \"BBEdit\" to open \"$_ddir\""
#     echo "Running command: $_cmd"
#     osascript -e "$_cmd"
#     cd "$_cdir"
# else
# if [ -d "$lastarg" ]; then
#     prj_file=$(find "$lastarg" -name *.bbprojectd | head -1)
#     if [ ! -e "${prj_file}" ]; then
#         prj_file="${dirpath%/*}/${dirpath##*/}.bbprojectd"
#         if [ ! -f "${prj_file}" ]; then
#             prj_file=""
#         fi
#     fi
#     if [ -e "${prj_file}" ]; then
#         echo "Project file: ${prj_file}"
#         read -p "Do you want to open project file? " yn
#         case $yn in
#             [Yy]* ) ${ED_CMD} "${prj_file}"; exit;;
#         esac
#         ${ED_CMD} --project "$lastarg"
#     fi
# else
#     case "$lastarg" in
#         *_EDITMSG|*MERGE_MSG|*_TAGMSG )
#             ${VIM_CMD} "$lastarg"
#             ;;
#         *crontab.* )
#             ${VIM_CMD} "$lastarg"
#             ;;
#         *.md|*.markdown|*.mdown|*.mkdown )
#             ${MARKDOWN_CMD} "$lastarg"
#             ;;
#         *.txt|*.log )
#             ${TXT_ED_CMD} "$lastarg"
#             ;;
#         * )
#             ${ED_CMD} "$lastarg"
#             ;;
#     esac
# fi
