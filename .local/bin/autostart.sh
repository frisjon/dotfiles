#!/bin/bash
$HOME/.local/bin/xcv mocha
xsetroot -solid "#fa0"
dunst &

[ ! -z $(prep -x bspwm) ] && $HOME/.local/bin/bspwm_bar.sh &
[ ! -z $(prep -x dwm) ] && $HOME/.local/bin/dwm_bar.sh &
