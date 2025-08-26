#!/bin/bash

DOTFILES_DIR=~/dotfiles/.emacs.d
EMACS_DIR=$(wslpath "$(wslvar USERPROFILE)")/AppData/Roaming/.emacs.d
SUBDIR_LIST="config lisp themes"

_diff_only () {
  /usr/bin/diff --color=auto $1 $2
}

_diff_and_copy_file_from_to () {
  from=$2
  to=$3
  from=${from//$DOTFILES_DIR/Wsl}
  from=${from//$EMACS_DIR/Win}
  to=${to//$DOTFILES_DIR/Wsl}
  to=${to//$EMACS_DIR/Win}
  /usr/bin/diff $2/$1 $3/$1 &> /dev/null
  if [ ! $? -eq 0 ] || [ ! -f $3/$1 ]; then
    /usr/bin/cp -i $2/$1 $3/$1
    echo -e "$from $1" '\t'"->" "$to $1"
  fi
}

_diff_and_copy () {
  from=$1
  to=$2
  from=${from//$DOTFILES_DIR\//Wsl }
  from=${from//$EMACS_DIR\//Win }
  to=${to//$DOTFILES_DIR\//Wsl }
  to=${to//$EMACS_DIR\//Win }

  echo -e "$from" '\t'"->" "$to"
  for file in $1/*; do
    file_dst=$(echo $file | rev | cut -d'/' -f 1 | rev)
    /usr/bin/diff $file $2/$file_dst &> /dev/null
    if [ ! $? -eq 0 ] || [ ! -f $2/$file_dst ]; then
      /usr/bin/cp -i $file $2/$file_dst
      echo "  $file"
    fi
  done
}

case $1 in
  "d2w"|"-d2w"|"--d2w")
    echo "Dotfiles to Windows"
    read -p "Are you sure? y/[n] " opt1
    case $opt1 in
      y|Y)
        _diff_and_copy_file_from_to init.el $DOTFILES_DIR $EMACS_DIR
        _diff_and_copy_file_from_to custom-vars.el $DOTFILES_DIR $EMACS_DIR
        for dir in $SUBDIR_LIST;do
          if [ ! -d $EMACS_DIR/$dir ]; then
            mkdir -p "$EMACS_DIR/$dir"
          fi
          _diff_and_copy "$DOTFILES_DIR/$dir" "$EMACS_DIR/$dir"
        done
    ;;
    n|N|*) echo "skipping...";;
    esac
  ;;
  "w2d"|"-w2d"|"--w2d")
    echo "Windows to Dotfiles"
    read -p "Are you sure? y/[n] " opt1
    case $opt1 in
      y|Y)
        _diff_and_copy_file_from_to init.el $EMACS_DIR $DOTFILES_DIR
        _diff_and_copy_file_from_to custom-vars.el $EMACS_DIR $DOTFILES_DIR
        for dir in $SUBDIR_LIST;do
          if [ ! -d $DOTFILES_DIR/$dir ]; then
            mkdir -p "$DOTFILES_DIR/$dir"
          fi
          _diff_and_copy "$EMACS_DIR/$dir" "$DOTFILES_DIR/$dir"
        done
    ;;
    n|N|*) echo "skipping...";;
    esac

  ;;
  "diff"|"-diff"|"--diff"|"-d")
    echo "diff Windows Dotfiles"
    for dir in $SUBDIR_LIST;do
      _diff_only "$DOTFILES_DIR/$dir" "$EMACS_DIR/$dir"
    done
  ;;
  *)
    echo no option
    echo choose from:
    echo "   diff, --diff, -d	diff config files between windows and dotfiles"
    echo "   w2d,  --w2d     	move windows config files to dotfiles"
    echo "   d2w,  --d2w     	move dotfiles config files to windows"
    ;;
esac

