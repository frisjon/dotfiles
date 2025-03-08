#!/bin/bash

# this script extracts the most used colors from an image
# returns a command to imagemagick to generate an image of said colors

nombre=$(date '+%Y%m%d-%H%M%S')

list=$(convert $1 -depth 16 +dither -colors 16 -unique-colors txt:- | tail -n +2 | sed -n 's/^.*\#.* \(.*\).*$/xc:\"\1\"/p' | sed -n 's/srgb/rgb/p' | tr '\r\n' ' ')

echo "convert -size 100x100 $list +append $nombre$(sed -n 's/\..*//g' <(echo $1)).png"
