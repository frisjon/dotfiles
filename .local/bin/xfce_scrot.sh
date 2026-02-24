#!/bin/bash
fname=$(date "+%Y%m%d_%H%M%S_%3N.png")
notify-send "Saved @ ~/Pictures/Screenshots/$fname"
case $1 in
"-r")xfce4-screenshooter -r -s $HOME/Pictures/Screenshots/$fname;;
*) xfce4-screenshooter -f -s $HOME/Pictures/Screenshots/$fname;;
esac
