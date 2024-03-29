@echo off

rem set current directory to folder in this script
cd /d %~dp0
set current=%CD%

set home=e:\home
set repopath=%current%

mklink %home%\.profile %repopath%\.profile
mklink %home%\.gitconfig %repopath%\.gitconfig
mklink %home%\.gitignore_global %repopath%\.gitignore_global
mklink %home%\.hgrc %repopath%\.hgrc
mklink %home%\.hgignore_global %repopath%\.hgignore_global
mklink /D %home%\.emacs.d %repopath%\.emacs.d
mklink %home%\.inputrc %repopath%\.inputrc
mklink %home%\.vimrc %repopath%\.vimrc
mklink %home%\.config/screenrc %repopath%\.config/screenrc
mklink %home%\.config\starship.toml %repopath%\.config\starship.toml
mklink %home%\.config\ripgrep.conf %repopath%\.config\ripgrep.conf
