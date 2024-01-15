#!/bin/bash

# This script is designed to bootstrap my dotfiles its taken from [[][chezmoi site]].
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply Luis-Henriquez-Perez
