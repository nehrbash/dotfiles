#!/bin/bash

if ! pgrep -f wlogout &> /dev/null 2>&1;
then
    wlogout -p layer-shell &
else
    pkill wlogout
fi


