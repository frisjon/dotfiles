#!/bin/bash

INTERACTIVE="-i"

while getopts 'fbh' opt;do
  case "$opt" in
    f) INTERACTIVE="-f"
    ;;
    b) BPATH=$(pwd)
       cd   dwm && sudo make clean install &> $BPATH/.tmp && [ $? -eq "0" ] && echo "dwm   OK" && cd ..
       cd    st && sudo make clean install &> $BPATH/.tmp && [ $? -eq "0" ] && echo "st    OK" && cd ..
       cd dmenu && sudo make clean install &> $BPATH/.tmp && [ $? -eq "0" ] && echo "dmenu OK" && cd ..
       rm $BPATH/.tmp
       echo Build OK
       exit 0
    ;;
    ?|h) echo "Usage: $(basename $0) [-f]"
         echo " -f    force"
         exit 1
    ;;
  esac
done
shift "$(($OPTIND-1))"

IGNORE="\./st\|\./dwm\|\./dmenu\|/.git\|/.gitignore\|readme\|\./xkb\|packages.txt\|setup.sh\|colors\|customo.ahk\|todo.md"
direcs=$(find -type d | grep -iv $IGNORE | cut -c3-)
for d in $direcs;do
  [ ! -d $HOME/$d ] && mkdir -p $HOME/$d && echo Create dir $HOME/$d
done

files=$(find . -type f | grep -iv $IGNORE | cut -c3-)
for f in $files;do
  ln -s $INTERACTIVE $(pwd)/$f $HOME/$f && [ $? -eq "0" ] && echo "$f OK"
done
