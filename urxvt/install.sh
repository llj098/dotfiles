#!/bin/bash

mv ~/.Xdefaults ~/.Xdefaults.old
cp .Xdefaults ~/
xrdb ~/.Xdefaults
sudo cp clipboard /usr/lib/urxvt/perl/
sudo pacman -S xclip

