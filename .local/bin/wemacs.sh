#!/bin/bash

DOTFILES_DIR=~/dotfiles/.emacs.d
EMACS_DIR=$(wslpath "$(wslvar USERPROFILE)")/AppData/Roaming/.emacs.d

_diff_and_copy () {
  echo "$1 -> $2"
  for file in $1/*.el; do
    file_dst=$(echo $file | rev | cut -d'/' -f 1 | rev)
    /usr/bin/diff $file $2/$file_dst &> /dev/null
    if [ ! $? -eq 0 ] && [ ! -f $1/../$file_dst ]; then
      #echo $1/../$file_dst
      /usr/bin/cp -i $file $2/$file_dst
    fi
  done
}

case $1 in
  "d2w")
    echo "Dotfiles to Windows"
    _diff_and_copy $DOTFILES_DIR $EMACS_DIR
    _diff_and_copy "$DOTFILES_DIR/lisp" "$EMACS_DIR/lisp"
  ;;
  "w2d")
    _diff_and_copy $EMACS_DIR $DOTFILES_DIR
    _diff_and_copy "$EMACS_DIR/lisp" "$DOTFILES_DIR/lisp"
  ;;
  *)
    echo no option
    ;;
esac

