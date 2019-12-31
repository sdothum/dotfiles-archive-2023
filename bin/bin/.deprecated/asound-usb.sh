#!/bin/sh
# set usb sound card which appears to be dynamically assigned on this system!
# see openbox "autostart"
# (there is a way to fix the card order assignment..)
set -x
card=$(aplay -l | grep "DAC \[USB Audio DAC\]" | sed 's/^card \(.\).*/\1/')
echo "audio card=${card}"
mv ~/.asoundrc ~/.asoundrc.last
sed "s/^card ./card ${card}/" ~/.asoundrc.template > ~/.asoundrc.tmp
mv ~/.asoundrc.tmp ~/.asoundrc 
cp ~/.config/volumeicon/volumeicon ~/.config/volumeicon/volumeicon.last
sed "s/^card=hw:./card=hw:${card}/" ~/.config/volumeicon/volumeicon.template > ~/.config/volumeicon/volumeicon.tmp
mv ~/.config/volumeicon/volumeicon.tmp ~/.config/volumeicon/volumeicon 
