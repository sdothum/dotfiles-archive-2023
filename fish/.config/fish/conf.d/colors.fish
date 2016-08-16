
# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ........................................................ ANSI terminal colours

set -x _blink_   (printf "\e[5m")
set -x _noblink_ (printf "\e[25m")
set -x _nocolor_ (printf "\e[0;0m")

# _normal_
# _BRIGHT_
set -x _black_   (printf "\e[0;30m")
set -x _BLACK_   (printf "\e[1;30m")
set -x _white_   (printf "\e[0;37m")
set -x _WHITE_   (printf "\e[1;37m")
set -x _blue_    (printf "\e[0;34m")
set -x _BLUE_    (printf "\e[1;34m")
set -x _cyan_    (printf "\e[0;36m")
set -x _CYAN_    (printf "\e[1;36m")
set -x _green_   (printf "\e[0;32m")
set -x _GREEN_   (printf "\e[1;32m")
set -x _magenta_ (printf "\e[0;35m")
set -x _MAGENTA_ (printf "\e[1;35m")
set -x _red_     (printf "\e[0;31m")
set -x _RED_     (printf "\e[1;31m")
set -x _yellow_  (printf "\e[0;33m")
set -x _YELLOW_  (printf "\e[1;33m")
