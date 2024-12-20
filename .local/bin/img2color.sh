#!/bin/bash

# Image 2 Colors: Extract 16 dominant colors from an image

#image="${1:-$IMAGE}"
read p
image="${1:-$p}"

# Taken from http://softwarerecs.stackexchange.com/a/14652

function get_colors(){
  convert "${1}" -colors 16 -depth 8 -format '%c' histogram:info:- \
    | sort --reverse --numeric-sort \
    | gawk 'match ($0, /^ *[0-9]+: \([^)]+\) (#[0-9A-F]+) .+$/, a) { print a[1] }'
}

while read line; do
  get_colors "${line}"
done < "${image:-/dev/stdin}"
