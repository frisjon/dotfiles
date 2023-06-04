#!/bin/bash

D1="%Y-%m-%d"
D2="%b %a"
H1="%H:%M"
H2="%H:%M:%S"

while true;do
  xsetroot -name "$(date "+$D1 | $H1")"
  sleep 2
done
