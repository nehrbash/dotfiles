#!/usr/bin/env bash

# Some countries below have more than one time zone,
# and it makes sense to use cities instead of countries.
# Use tzselect to find their time zones.

time=$(TZ='America/Fortaleza' date +'%H:%M:%S')
echo '<span size="25000" foreground="#d193e3" face="monospace" weight="bold">Brazil '$time'</span>'

echo '<span size="large" face="monospace" foreground="#a9a1e1">'
time=$(TZ='America/Atikokan' date +"%H:%M")
echo 'Canada	<b>'$time'</b>'

time=$(TZ='Europe/Dublin' date +"%H:%M")
echo 'Ireland	<b>'$time'</b>'

time=$(TZ='Asia/Kolkata' date +"%H:%M")
echo 'India	<b>'$time'</b>'

time=$(TZ='Asia/Shanghai' date +"%H:%M")
echo 'China	<b>'$time'</b>'
echo '</span>'
