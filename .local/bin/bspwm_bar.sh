#!/bin/bash
DESKTOPS=$(bspc query -D --names | xargs)
bspc subscribe desktop_focus | while read line; do
  $HOME/.local/bin/bspwm_status.sh
done
