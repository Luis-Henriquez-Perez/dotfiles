#!/bin/bash

# Filename: setup_systemd_services.sh
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-17 05:35:08
# Description: Initialize systemd services.
# For a new system, this is to.

# List of services to enable and start
services=(
    lightdm.service
    NetworkManager.service
    ntpd.service
)

echo "Starting systemd service setup..."

for service in "${services[@]}"; do
    echo "Processing $service..."

    # Enable the service at boot
    if systemctl enable "$service" 2>/dev/null; then
        echo "Enabled $service successfully."
    else
        echo "Failed to enable $service. Skipping..."
        continue
    fi

    # Start the service immediately
    if systemctl start "$service" 2>/dev/null; then
        echo "Started $service successfully."
    else
        echo "Failed to start $service. It might already be running."
    fi
done

echo "Systemd service setup completed."
