sudo pacman -S ttf-roboto noto-fonts noto-fonts-cjk adobe-source-han-sans-cn-fonts adobe-source-han-serif-cn-fonts ttf-dejavu

sudo cp /etc/fonts/fonts.conf /etc/fonts/fonts.bak.conf
sudo cp fonts.android.conf /etc/fonts/fonts.conf
sudo cp ../font/* /usr/share/fonts/TTF/
fc-cache -f -v 
