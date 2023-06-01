#!/bin/bash

if [ -d /usr/share/X11/xkb/ ] && [ -f ./customo ] && [ -f evdev.xml ];then
  sudo cp -i ./customo /usr/share/X11/xkb/symbols
  sudo cp -i ./evdev.xml /usr/share/X11/xkb/rules/evdev.xml
  # debian
  sudo dpkg-reconfigure xkb-data
  setxkbmap customo
else
  echo "Failed. Either /usr/share/X11/xkb/ is not a dir or files ./customo ./evdev.xml do not exist"
fi
echo "Done"
