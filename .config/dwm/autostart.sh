#! /bin/bash 
picom -b --experimental-backends &
nitrogen --restore &
killall "slstatus"
slstatus &
killall "xiccd"
xiccd & 
notify-osd &
polkit-gnome-authentication-agent-1 &
#ppwire &
xfce4-power-manager &
killall "unclutter"
