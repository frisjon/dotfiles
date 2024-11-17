#!/usr/bin/python3.8

# 1st arg is factor
#  just a number
# 2nd..n arg is string of colors to be applied factor
#  format of color is #rrggbb (symbol # is optional, but remember to input param as string with " or ')
#  factor is applied to each channel individually


from sys import argv

def funca(color, fac):
    color = color.replace('#','')
    red   = int(int(color[0:2],base=16)*fac)
    green = int(int(color[2:4],base=16)*fac)
    blue  = int(int(color[4:6],base=16)*fac)
    r = hex(255 if (red  )>255 else red  ).replace('0x','').rjust(2,'0')
    g = hex(255 if (green)>255 else green).replace('0x','').rjust(2,'0')
    b = hex(255 if (blue )>255 else blue ).replace('0x','').rjust(2,'0')
    s = '#' + r + g + b
    return s

#for arch in argv[1:]:
#    ivo = open(arch, 'r')
#    for line in ivo:
#        stdout.write(line.replace('\n',' '))
#        i = line.split()
#        for j in range(1, 5):
#            stdout.write('#' + funca(i[2], 1 + j / 10) + ' ')
#        stdout.write('\n')
#    stdout.write('\n')
#    ivo.close()

for i in argv[2:]:
    print(i,'->',funca(i,float(argv[1])))
