
export XMODIFIERS=@im=fcitx
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim

autocutsel -selection PRIMARY -fork &
VBoxClient-all &
exec herbstluftwm
