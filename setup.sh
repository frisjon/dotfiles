#!/bin/bash

INTERACTIVE="-i"

while getopts 'oh' opt;do
  case "$opt" in
    o) INTERACTIVE=""
    ;;
    ?|h) echo "Usage: $(basename $0) [-o]"
         echo " -o    no interactive mode"
         exit 1
    ;;
  esac
done
shift "$(($OPTIND-1))"

if [ ! -d $HOME/.local/bin ]; then
  mkdir $HOME/.local/bin
fi

cmd="cp $INTERACTIVE .local/bin/* $HOME/.local/bin"
eval $cmd

if [ ! -d $HOME/.config ]; then
  mkdir $HOME/.config
fi

cmd="cp $INTERACTIVE -r .config/* $HOME/.config"
eval $cmd

dfiles=".inputrc .bashrc .aliases .profile .xinitrc .gitconfig .Xresources"

for f in $dfiles;do
  cmd="ln -s $INTERACTIVE $(pwd)/$f $HOME"
  eval $cmd
done
