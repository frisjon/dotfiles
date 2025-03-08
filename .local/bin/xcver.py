#/usr/bin/python

import sys
import subprocess
import datetime

def funca(color, fac):
    color = color.replace('#','')
    r = hex(int(int(color[0:2], base=16)*fac)).replace('0x','').rjust(2,'0')
    g = hex(int(int(color[2:5], base=16)*fac)).replace('0x','').rjust(2,'0')
    b = hex(int(int(color[5:], base=16) *fac)).replace('0x','').rjust(2,'0')
    s = r + g + b
    return s

#today = datetime.datetime.now().strftime('%Y%m%d-%H%M%S')
print("convert " + sys.argv[1] + " -depth 8")
exit(0)
#command = "convert " + sys.argv[1] + " -depth 8 +dither -colors 16 -unique-colors txt:-"
command = "convert p.jpg -depth 8 +dither -colors 16 -unique-colors txt:-"

process = subprocess.Popen(command.split(), stdout=subprocess.PIPE)
output, error = process.communicate()

l = output.split()
l = l[5:]
hexvalues=[]
srgb=[]

for i in range(0,len(l),4):
    hexvalues.append(str(l[i+2]).replace("b'",'').replace("'",""))
    srgb.append(l[i+3])

hexvalues.sort()

colors = ["Black","Red","Green","Yellow","Blue","Magenta","Cyan","White"]

tema = '#define FOREGROUND #f2f2f2\n\
#define CURSOR #FOREGROUND
#define BACKGROUND #2b2b2b\n\n'

j=0
for i in hexvalues:
    tema += '#define ' + colors[j] + ' ' + i + '\n'
    j += 1

j=0
for i in hexvalues:
    tema += '#define br' + colors[j] + ' ' + i + '\n'
    j += 1

#tema += '\n\
#color0="$black"\n\
#color1="$red"\n\
#color2="$green"\n\
#color3="$yellow"\n\
#color4="$blue"\n\
#color5="$magenta"\n\
#color6="$cyan"\n\
#color7="$white"\n\
#color8="$brblack"\n\
#color9="$brred"\n\
#color10="$brgreen"\n\
#color11="$bryellow"\n\
#color12="$brblue"\n\
#color13="$brmagenta"\n\
#color14="$brcyan"\n\
#color15="$brwhite"\n\
#\n\
#cursor="$foreground"\n\
#border="$background"\n\
#highlight="$foreground"\n\
#mouse_background="$black"\n\
#mouse_foreground="$brcyan"'

f = open('asd','w')
f.write(tema)
f.close()

#print(funca('#ff0124',0.1))
