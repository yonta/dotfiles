@echo off

rem set current directory to folder in this script
cd /d %~dp0
set current=%CD%

set home=e:\home
set repopath=%current%

mklink %home%\.profile %repopath%\bash-win\.profile
mklink %home%\.gitconfig %repopath%\git\.gitconfig
mklink %home%\.gitignore_global %repopath%\git\.gitignore_global
mklink %home%\.hgrc %repopath%\mercurial\.hgrc
mklink %home%\.hgignore_global %repopath%\mercurial\.hgignore_global
mklink /D %home%\.emacs.d %repopath%\emacs
