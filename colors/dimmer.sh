#!/bin/bash
# 1st param: 'theme' file
# 2nd param: float : factor to apply to each color

./apply-factor-to-color.py $2 $(./take_colors.sh $1)
