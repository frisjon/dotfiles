#!/bin/bash
$HOME/.local/bin/xcv mocha
xsetroot -solid "#111222"
dunst &

[ ! -z $(pgrep -x bspwm) ] && $HOME/.local/bin/bspwm_bar.sh &
[ ! -z $(pgrep -x dwm) ] && $HOME/.local/bin/dwm_bar.sh &
