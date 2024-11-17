#!/bin/sh

bspc subscribe desktop_focus | while read line; do
      ~/.local/bin/notif 1
done
