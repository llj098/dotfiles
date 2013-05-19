export XMODIFIERS=@im=fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx

autocutsel -selection PRIMARY -fork &
VBoxClient-all &
exec herbstluftwm
