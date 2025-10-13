#!/usr/bin/bash

set -euxo pipefail

# this script launches an instance of emacsclient
# if the emacs server is not running, it executes it

[[ -z $(ps cax | grep emacs) ]] && emacs --daemon 
emacsclient -c
