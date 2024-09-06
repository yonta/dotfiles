#!/bin/bash

# absolute script dir
script_dir=`realpath $(dirname $0)`

# Help message
if [ -z ${1} ] || ([ ${1} != "copy" ] && [ ${1} != "force" ]); then
    echo "Usage: sym_link.sh [copy|force]"
    echo "    copy    do not overwrite, copy original file with '.orig'"
    echo "    force   overwrite link"
    exit
fi

# arg1: file (full path)
function copy_original () {
    if [ -e "${1}" ] || [ -L "${1}" ]; then
        echo "Message: copy existing \"${1}\" to \"${1}.orig\""
        mv ${1} ${1}.orig
    fi
}

# arg1: file (full path)
# The pattern of '-e' is file or directory, and '-L' is symbolic link.
function remove_original () {
    if [ -e "${1}" ] || [ -L "${1}" ]; then
        echo "Message: remove existing \"${1}\""
        rm -rf ${1}
    fi
}

# arg1: file or dir (full path), arg2: target dir (full path)
function make_link () {
    if [ -f "${1}" ]; then
        ln -s ${1} ${2}
    elif [ -d "${1}" ]; then
        ln -s -d ${1} ${2}
    fi
}

# arg1: file (abs path), arg2: "copy" or "force"
function nice_make_link () {
    if [ "${2}" = "copy" ]; then
        copy_original ${HOME}/${1}
    elif [ "${2}" = "force" ]; then
        remove_original ${HOME}/${1}
    fi
    make_link ${script_dir}/${1} ${HOME}/${1}
}

# $1はcopy or force
files="\
.config/bash .config/emacs .config/git .config/hg .config/solargraph \
.config/bat \
.bashrc .profile .vimrc \
.config/screenrc .config/starship.toml .config/ripgrep.conf \
"
for file in ${files}
do
    nice_make_link ${file} ${1}
done
