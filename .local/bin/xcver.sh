#!/bin/bash

func() {
    r=$(echo $(( 16#${1:1:2} ))*$2 | bc | cut -f1 -d".")
    g=$(echo $(( 16#${1:3:2} ))*$2 | bc | cut -f1 -d".")
    b=$(echo $(( 16#${1:5:2} ))*$2 | bc | cut -f1 -d".")
    printf "#%02x%02x%02x\n" $r $g $b
}

name="$(date '+%Y%m%d-%H%M%S')"
numcolors=8
listo="$(convert $1 -depth 8 +dither -colors $numcolors -unique-colors txt:- | tail -n +2 | sed -n 's/^.*\#\(.*\) .*$/\1/p')"
#lista=$(convert $1 -depth 8 +dither -colors $numcolors -unique-colors txt:- | tail -n +2 | sed -n 's/^.*\#.* \(.*\).*$/xc:\"\1\"/p' | sed -n 's/srgb/rgb/p' | tr '\r\n' ' ')

echo > tmp
for i in $listo;do echo $i>>tmp;done
listo=""
for i in $(sort tmp);do listo+="#$i ";done

colorn[0]="Black"
colorn[1]="Red"
colorn[2]="Green"
colorn[3]="Yellow"
colorn[4]="Blue"
colorn[5]="Magenta"
colorn[6]="Cyan"
colorn[7]="White"

echo "$(echo $listo | cut -f 1 -d" ")"

colorv[0]=
colorv[1]=
colorv[2]=
colorv[3]=
colorv[4]=
colorv[5]=
colorv[6]=
colorv[7]=

j=0
for i in $listo;do
    colorv[$j]=$i
    #echo "#define   ${colorn[$j]} $i"
    #echo "#define br${colorn[$j]} $(func $i 0.9)"
    j=$((j+1))
done

echo "#define FOREGROUND $"
echo "#define CURSOR #f2f2f2"
echo "#define BACKGROUND #2b2b2b"
