#!/bin/bash

# Function to install packages via pacman
install_pacman_packages() {
    for package in "${pacman_packages[@]}"; do
        if ! pacman -Qi "$package" &> /dev/null; then
            echo "Installing $package with pacman..."
            sudo pacman -S --noconfirm --needed "$package"
        else
            echo "$package is already installed."
        fi
    done
}

install_aur_packages() {
    for package in "${aur_packages[@]}"; do
        if ! yay -Qi "$package" &> /dev/null; then
            echo "Installing $package with yay..."
            yay -S --noconfirm "$package"
        else
            echo "$package is already installed."
        fi
    done
}

OS="$(uname -s)"
case "${OS}" in
    Linux*)
        if [ -f /etc/fedora-release ]; then
            install_on_fedora
        elif [ -f /etc/lsb-release ]; then
            install_on_ubuntu
        else
            echo "Unsupported Linux distribution"
            exit 1
        fi
        ;;
    Darwin*)
        install_on_mac
        ;;
    *)
        echo "Unsupported operating system: ${OS}"
        exit 1
        ;;
esac
