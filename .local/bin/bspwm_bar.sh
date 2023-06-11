#!/bin/bash
DESKTOPS=$(bspc query -D --names | xargs)
bspc subscribe desktop_focus | while read line; do
  #DESKTOPID=$(echo $line | cut -d' ' -f3)
  FOCUSED=$(bspc query -D -d .focused --names)
  OCCUPIED=$(bspc query -D -d .occupied --names)
  dunstify -r 1001 "$DESKTOPS"
done
