
# Zsh Environment
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ........................................................ ANSI terminal colours

# simpler usage than zsh $fg color array
export _blink_=$(printf "\e[5m")
export _noblink_=$(printf "\e[25m")
export _nocolor_=$(printf "\e[0;0m")

# _normal_
# _BRIGHT_
export _black_=$(printf "\e[0;30m")
export _BLACK_=$(printf "\e[1;30m")
export _white_=$(printf "\e[0;37m")
export _WHITE_=$(printf "\e[1;37m")
export _blue_=$(printf "\e[0;34m")
export _BLUE_=$(printf "\e[1;34m")
export _cyan_=$(printf "\e[0;36m")
export _CYAN_=$(printf "\e[1;36m")
export _green_=$(printf "\e[0;32m")
export _GREEN_=$(printf "\e[1;32m")
export _magenta_=$(printf "\e[0;35m")
export _MAGENTA_=$(printf "\e[1;35m")
export _red_=$(printf "\e[0;31m")
export _RED_=$(printf "\e[1;31m")
export _yellow_=$(printf "\e[0;33m")
export _YELLOW_=$(printf "\e[1;33m")
