#!/bin/bash
# Fonts
apt update
apt install -y fonts-lato fonts-noto fonts-roboto fonts-ubuntu fonts-open-sans

# R-packages
install2.r --error --skipinstalled --repo https://nexus.kela.fi/repository/r-public/ \
    golem



echo -e "\nInstall kelaspecs, done!"
