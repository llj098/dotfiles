



sudo timedatectl set-local-rtc 0
sudo timedatectl set-timezone Asia/Chongqing

sudo pacman -S ntp
sudo ntpd -qg
sudo hwclock -w

sudo pacman -S ttf-inconsolata dina-font

