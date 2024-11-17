#!/bin/bash

THEME=$1

trash-put /tmp/my_light_colors.png /tmp/my_dark_colors.png /tmp/my_image.png

#light colors
convert -size 50x50 +append $(grep 'COLOR[0-7] ' $THEME | tr -s ' ' | cut -d'#' -f3 | sed 's/^/xc:#/g' | xargs) /tmp/my_light_colors.png

#dark colors
convert -size 50x50 +append $(grep 'COLOR[89]\|COLOR1[0-5]' $THEME | tr -s ' ' | cut -d'#' -f3 | sed 's/^/xc:#/g' | xargs) /tmp/my_dark_colors.png

convert /tmp/my_light_colors.png /tmp/my_dark_colors.png -append /tmp/my_image.png && imagemap /tmp/my_image.png
