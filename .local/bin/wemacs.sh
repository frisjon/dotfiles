#!/bin/bash

DOTFILES_DIR=~/dotfiles/.emacs.d
EMACS_DIR=$(wslpath "$(wslvar USERPROFILE)")/AppData/Roaming/.emacs.d
SUBDIR_LIST="config lisp themes"

_diff_only () {
  /usr/bin/diff --color=auto $1 $2
}

_diff_and_copy_file_from_to () {
  echo "$2/$1 -> $3/$1"
  /usr/bin/diff $2/$1 $3/$1 &> /dev/null
  if [ ! $? -eq 0 ] || [ ! -f $3/$1 ]; then
    /usr/bin/cp -i $2/$1 $3/$1
  fi
}

_diff_and_copy () {
  echo "$1 -> $2"
  for file in $1/*; do
    file_dst=$(echo $file | rev | cut -d'/' -f 1 | rev)
    /usr/bin/diff $file $2/$file_dst &> /dev/null
    if [ ! $? -eq 0 ] || [ ! -f $2/$file_dst ]; then
      /usr/bin/cp -i $file $2/$file_dst
    fi
  done
}

case $1 in
  "d2w")
    echo "Dotfiles to Windows"
    _diff_and_copy_file_from_to init.el $DOTFILES_DIR $EMACS_DIR
    _diff_and_copy_file_from_to custom-vars.el $DOTFILES_DIR $EMACS_DIR
    for dir in $SUBDIR_LIST;do
      if [ ! -d $EMACS_DIR/$dir ]; then
        mkdir -p "$EMACS_DIR/$dir"
      fi
      _diff_and_copy "$DOTFILES_DIR/$dir" "$EMACS_DIR/$dir"
    done
  ;;
  "w2d")
    _diff_and_copy_file_from_to init.el $EMACS_DIR $DOTFILES_DIR
    _diff_and_copy_file_from_to custom-vars.el $EMACS_DIR $DOTFILES_DIR
    for dir in $SUBDIR_LIST;do
      if [ ! -d $DOTFILES_DIR/$dir ]; then
        mkdir -p "$DOTFILES_DIR/$dir"
      fi
      _diff_and_copy "$EMACS_DIR/$dir" "$DOTFILES_DIR/$dir"
    done
  ;;
  "diff")
    echo "Local v Dotfiles"
    for dir in $SUBDIR_LIST;do
      _diff_only "$DOTFILES_DIR/$dir" "$EMACS_DIR/$dir"
    done
  ;;
  *)
    echo no option
    ;;
esac

