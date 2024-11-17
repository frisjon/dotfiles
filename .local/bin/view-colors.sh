#!/bin/bash

# in: list of colors in rgb format
# out: command to generate a color palette

list=""
for i in $@;do
    list+="xc:\"#$i\" "
done

echo "convert -size 100x100 $list +append out.png"
