#!/bin/bash

# read image

echo $1

size=${#1}
size=$(($size-4))

img=$(echo "$1" | cut -c 1-$size)

convert "$1" -colors 8 -depth 8 -format '%c' -unique-colors histogram:info:- \
    | sort --reverse --numeric-sort \
    | gawk 'match ($0, /^ *[0-9]+: \([^)]+\) (#[0-9A-F]+) .+$/, a) { print a[1] }' \
    | tee "$img-palette.txt" \
    | while read colour; do convert -size 20x20 "xc:$colour" +depth miff:-; done \
    | montage - -geometry +0+0 "$img-palette.png"
