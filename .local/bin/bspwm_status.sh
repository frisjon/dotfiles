#!/bin/bash

DESKTOPS=$(bspc query -D --names | xargs)
FOCUSED=$(bspc query -D -d .focused --names)
OCCUPIED=$(bspc query -D -d .occupied --names)

OUT=""

for i in $DESKTOPS; do
    if [ $i -eq $FOCUSED ]; then
        OUT="$OUT<$i>"
    elif [ ! -z "$(echo $OCCUPIED | grep $i)" ]; then
        OUT="$OUT $i*"
    else
        OUT="$OUT $i "
    fi
done

dunstify -r 1001 "$OUT"
