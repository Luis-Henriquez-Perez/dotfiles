#!/bin/bash
# Filename: reset_usb.sh
# Author: Luis Henriquez-Perez <luis@luishp.xyz>
# Created: 2024-12-29 18:29:56
# Description: Take a drive with an ISO make it into a normal usb again.

# Exit on error
set -e

# Check if run as root
# if [ "$(id -u)" -ne 0 ]; then
#   echo "This script must be run as root."
#   exit 1
# fi

# Prompt for the USB device
echo "Available drives:"
lsblk -o NAME,SIZE,TYPE | grep disk
printf "Enter the device name (e.g., sdb): "
read DEVICE
echo "You have chosen /dev/$DEVICE"

# DEVICE_PATH="/dev/$DEVICE"

# Ensure the device exists
if [ ! -b "$DEVICE_PATH" ]; then
  echo "Device $DEVICE_PATH does not exist."
  exit 1
fi

# Confirm the selected device
printf "WARNING: All data on $DEVICE_PATH will be lost. Proceed? (y/N): "
read CONFIRM
# if [ "$CONFIRM" != "y" ] && [ "$CONFIRM" != "Y" ]; then
#   echo "Operation canceled."
#   exit 1
# fi

# # Wipe the partition table
# echo "Wiping existing partition table on $DEVICE_PATH..."
# wipefs -a "$DEVICE_PATH"

# # Create a new partition table
# echo "Creating a new partition table (msdos)..."
# parted -s "$DEVICE_PATH" mklabel msdos

# # Create a new partition
# echo "Creating a new FAT32 partition..."
# parted -s "$DEVICE_PATH" mkpart primary fat32 0% 100%

# # Format the partition
# PARTITION="${DEVICE_PATH}1"
# echo "Formatting the partition as FAT32..."
# mkfs.vfat "$PARTITION"

# echo "USB drive $DEVICE_PATH has been restored to normal."
