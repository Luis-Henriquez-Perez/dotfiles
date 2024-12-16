#!/bin/bash

chosen=$(echo -e "[Cancel]\nShutdown\nLogout\nReboot\nSuspend\nHibernate\nHybrid-sleep\nSuspend-then-hibernate" | rofi -dmenu -i -no-fixed-num-lines -p "What do you want to do?")

if [[ $chosen = "Shutdown" ]]; then
    systemctl poweroff
elif [[ $chosen = "Logout" ]]; then
    jwm -exit
elif [[ $chosen = "Reboot" ]]; then
    systemctl reboot
elif [[ $chosen = "Suspend" ]]; then
    systemctl suspend
elif [[ $chosen = "Hibernate" ]]; then
    systemctl hibernate
elif [[ $chosen = "Hybrid-sleep" ]]; then
    systemctl hibernate
elif [[ $chosen = "Suspend-then-hibernate" ]]; then
    systemctl suspend-then-hibernate
fi
