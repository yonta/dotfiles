@echo off

rem set current directory to folder in this script
cd /d %~dp0
set current=%CD%

set home=e:\home
set hgpath=%home%\hg\config

mklink %home%\.profile %hgpath%\bash\.profile
mklink %home%\.gitconfig %hgpath%\git\.gitconfig
mklink %home%\.gitignore_global %hgpath%\git\.gitignore_global
mklink %home%\.hgrc %hgpath%\mercurial\.hgrc
mklink %home%\.hgignore_global %hgpath%\mercurial\.hgignore_global
mklink /D %home%\.emacs.d %hgpath%\emacs
