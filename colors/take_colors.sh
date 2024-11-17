#!/bin/bash
grep 'COLOR[0-7] ' $1 | tr -s ' ' | cut -d' ' -f3 | cut -d'#' -f2 | xargs 
