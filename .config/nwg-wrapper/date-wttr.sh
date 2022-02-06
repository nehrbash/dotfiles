#!/usr/bin/env bash

time=$(LC_ALL=C date +'%A, %d. %B')
wttr=$(curl https://wttr.in/?format=1)
echo '<span size="45000" foreground="#a9a1e1">'$time'</span><span size="35000" foreground="#9ca0a4">'
echo $wttr'</span>'
