#!/bin/bash

feh --bg-fill $1 

name="$(date '+%Y%m%d-%H%M%S')"

numcolors=8

#list="$(convert $1 -depth 8 +dither -colors $numcolors -unique-colors txt:- | tail -n +2 | sed -n 's/^.*\#\(.*\) .*$/#\1/p')"

listo="$(convert $1 -depth 8 +dither -colors $numcolors -unique-colors txt:- | tail -n +2 | sed -n 's/^.*\#\(.*\) .*$/\1/p')"

lista=$(convert $1 -depth 8 +dither -colors $numcolors -unique-colors txt:- | tail -n +2 | sed -n 's/^.*\#.* \(.*\).*$/xc:\"\1\"/p' | sed -n 's/srgb/rgb/p' | tr '\r\n' ' ')

echo > tmp
for i in $listo;do echo $i>>tmp;done
listo=""
for i in $(sort  tmp);do listo+="#$i ";done

color[0]="black"
color[1]="red"
color[2]="green"
color[3]="yellow"
color[4]="blue"
color[5]="magenta"
color[6]="cyan"
color[7]="white"

tema="\
foreground=\"#ffffff\"
background=\"#101010\"    
"

j=0
for i in $listo;do
    tema+="${color[$j]}=\"$i\"
"
    j=$((j+1))
done

j=0
for i in $listo;do
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
highlight=\"\$foreground\"
mouse_background=\"\$black\"
mouse_foreground=\"\$brcyan\"
"

echo "$tema" > ~/src/dynamic-colors/colorschemes/$name.sh

echo "convert -size 100x100 $lista +append $name.png"

~/.local/bin/change-theme $name
