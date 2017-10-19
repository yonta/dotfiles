#!/bin/bash

# Help message
if [ -z ${1} ] || ([ ${1} != "copy" ] && [ ${1} != "force" ]); then
    echo "Usage: sym_link.sh [copy|force]"
    echo "    copy    do not overwrite, copy original file with '.orig'"
    echo "    force   overwrite link"
    exit
fi

# arg1: file, arg2: target dir
function copy_original () {
    if [ -e ${2}/${1} ]; then
        echo "Message: exist \"${2}/${1}\", copy to \"${1}.orig\""
        mv ${2}/${1} ${2}/${1}.orig
    fi
}

function remove_original () {
    if [ -e ${2}/${1} ]; then
        echo "Message: exist \"${2}/${1}\", remove"
        rm -rf ${2}/${1}
    fi
}

# arg1: file, arg2: target dir
function make_file_link () {
    echo "Message: make link to \"${1}\""
    ln -s ${1} ${2}
}

# arg1: file, arg2: target dir
function make_dir_link () {
    echo "Message: make link to \"${1}\""
    ln -s -d ${1} ${2}
}

# Bash
if [ ${1} = "copy" ]; then
    copy_original .bashrc ${HOME}
elif [ ${1} = "force" ]; then
    remove_original .bashrc ${HOME}
fi
make_dir_link ${PWD}/bash-ubuntu/.bashrc ${HOME}/.bashrc

# Git
git_srcs=".gitconfig .gitignore_global"
for file in ${git_srcs}
do
    if [ ${1} = "copy" ]; then
        copy_original ${file} ${HOME}
    elif [ ${1} = "force" ]; then
        remove_original ${file} ${HOME}
    fi
    make_file_link ${PWD}/git/${file} ${HOME}
done

# Mercurial
mercurial_srcs=".hgignore_global .hgrc"
for file in ${mercurial_srcs}
do
    if [ ${1} = "copy" ]; then
        copy_original ${file} ${HOME}
    elif [ ${1} = "force" ]; then
        remove_original ${file} ${HOME}
    fi
    make_file_link ${PWD}/mercurial/${file} ${HOME}
done

# Emacs
if [ ${1} = "copy" ]; then
    copy_original .emacs.d ${HOME}
elif [ ${1} = "force" ]; then
    remove_original .emacs.d ${HOME}
fi
make_dir_link ${PWD}/emacs ${HOME}/.emacs.d
