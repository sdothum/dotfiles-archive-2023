# /etc/profile.d/freetype2.sh
# see .xinitrc -> . /etc/profile
[ $USER = root ] && exit
export FT2_SUBPIXEL_HINTING=1
