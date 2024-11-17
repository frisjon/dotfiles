#!/bin/bash

# in: image
# out: new colorscheme
# grabs colors from image
# generates a file for a program called 'dynamic-colors'
# that program can change terminal colorscheme

# first i should need to check that the parameter 
# passed is indeed an image

# set background image
feh --bg-tile $1 

list="$(convert $1 -depth 8 +dither -colors 10 -unique-colors txt:- | tail -n +2 | sed -n 's/^.*\#\(.*\) .*$/#\1/p')"

lista=$(convert $1 -depth 8 +dither -colors 10 -unique-colors txt:- | tail -n +2 | sed -n 's/^.*\#.* \(.*\).*$/xc:\"\1\"/p' | sed -n 's/srgb/rgb/p' | tr '\r\n' ' ')

# for reverse purposes
# i assume that 'list' is already sorted by 'convert'
# which happens to be the case
list=$(echo $list | awk '{ for (i=NF; i>1; i--) printf("%s ",$i); print $1; }')

color[0]="foreground"
color[1]="black"
color[2]="red"
color[3]="green"
color[4]="yellow"
color[5]="blue"
color[6]="magenta"
color[7]="cyan"
color[8]="white"
color[9]="background"

#tema="\
#foreground=\"#ffffff\"
#background=\"#101010\"    
#"

j=0
for i in $list;do
    tema+="${color[$j]}=\"$i\"
"
    j=$((j+1))
done

j=0
for i in $list;do
    tema+="br${color[$j]}=\"$i\"
"
    j=$((j+1))
done

tema+="\
color0=\"\$black\"
color1=\"\$red\"
color2=\"\$green\"
color3=\"\$yellow\"
color4=\"\$blue\"
color5=\"\$magenta\"
color6=\"\$cyan\"
color7=\"\$white\"
color8=\"\$brblack\"
color9=\"\$brred\"
color10=\"\$brgreen\"
color11=\"\$bryellow\"
color12=\"\$brblue\"
color13=\"\$brmagenta\"
color14=\"\$brcyan\"
color15=\"\$brwhite\"

cursor=\"\$foreground\"
border=\"\$background\"
highlight=\"\$black\"
mouse_background=\"\$black\"
mouse_foreground=\"\$brcyan\"
"

echo "$tema" #> ~/src/dynamic-colors/colorschemes/new.sh

echo "convert -size 100x100 $lista +append new_img_gen.png"

#~/.local/bin/change-theme new
