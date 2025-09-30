#!/bin/bash
$HOME/.local/bin/xcv manjaroi3
#xsetroot -solid "#111222"
[[ ! -z $(which nitrogen 2> /dev/null) ]] && nitrogen --restore
setxkbmap customo
#brightnessctl 30%
#dunst &
[ ! -z $(pgrep -x bspwm) ] && $HOME/.local/bin/bspwm_bar.sh &
[ ! -z $(pgrep -x dwm) ] && $HOME/.local/bin/dwm_bar.sh &
