#!/bin/bash

INTERACTIVE="-i"

while getopts 'fh' opt;do
  case "$opt" in
    f) INTERACTIVE="-f"
    ;;
    ?|h) echo "Usage: $(basename $0) [-f]"
         echo " -f    force"
         exit 1
    ;;
  esac
done
shift "$(($OPTIND-1))"

IGNORE="/st\|/dwm\|/dmenu\|/.git\|/.gitignore\|readme\|/xkb\|packages.txt\|setup.sh"
direcs=$(find -type d | grep -iv $IGNORE | cut -c3-)
for d in $direcs;do
  [ ! -d $HOME/$d ] && mkdir -p $HOME/$d && echo Create dir $HOME/$d
done

files=$(find . -type f | grep -iv $IGNORE | cut -c3-)
for f in $files;do
  echo -n "Linking $f"
  ln -s $INTERACTIVE $(pwd)/$f $HOME/$f
  [ $? -eq "0" ] && echo " OK"
done
