#!/bin/bash

INTERACTIVE="-i"

while getopts 'oh' opt;do
  case "$opt" in
    o) INTERACTIVE="-f"
    ;;
    ?|h) echo "Usage: $(basename $0) [-o]"
         echo " -o    no interactive mode"
         exit 1
    ;;
  esac
done
shift "$(($OPTIND-1))"

echo Linking ~/.local/bin
if [ ! -d $HOME/.local/bin ]; then
  mkdir $HOME/.local/bin;
fi

for f in $(pwd)/.local/bin/*;do
  ln -s $INTERACTIVE $f $HOME/.local/bin
  echo " $f"
done

echo Linking ~/.config/xres/themes
if [ ! -d $HOME/.config/xres/themes ]; then
  mkdir -p $HOME/.config/xres/themes
fi

for f in $(pwd)/.config/xres/themes/*;do
  ln -s $INTERACTIVE $f $HOME/.config/xres/themes
  echo " $f"
done

echo Linking dotfiles
dfiles=".inputrc .bashrc .aliases .profile .xinitrc .gitconfig .Xresources"
for f in $dfiles;do
  ln -s $INTERACTIVE $(pwd)/$f $HOME
  echo " $f"
done
