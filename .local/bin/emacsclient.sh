#!/usr/bin/bash

# this script launches an instance of emacsclient
# if the emacs server is not running, it executes it

ps cax | grep emacs > /dev/null
if [ $? -ne 0 ]; then
    emacs --daemon
fi

emacsclient -c &
