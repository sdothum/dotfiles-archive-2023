# sdothum - 2016 (c) wtfpl

# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ........................................................ ANSI terminal colours

# for posix shell scripts
# blink|noblink|nocolor|normal|underline
set -x BLINK     (printf "\e[5m")
set -x NOBLINK   (printf "\e[25m")
set -x NOCOLOR   (printf "\e[0;0m")
set -x UNDERLINE (printf "\e[4m")

# NORMAL
# __BRIGHT
# black|BLACK|white|WHITE|blue|BLUE|cyan|CYAN|green|GREEN|magenta|MAGENTA|red|RED|yellow|YELLOW
set -x BLACK     (printf "\e[0;30m")
set -x __BLACK   (printf "\e[1;90m")
set -x WHITE     (printf "\e[0;37m")
set -x __WHITE   (printf "\e[1;97m")
set -x BLUE      (printf "\e[0;34m")
set -x __BLUE    (printf "\e[1;94m")
set -x CYAN      (printf "\e[0;36m")
set -x __CYAN    (printf "\e[1;96m")
set -x GREEN     (printf "\e[0;32m")
set -x __GREEN   (printf "\e[1;92m")
set -x MAGENTA   (printf "\e[0;35m")
set -x __MAGENTA (printf "\e[1;95m")
set -x RED       (printf "\e[0;31m")
set -x __RED     (printf "\e[1;91m")
set -x YELLOW    (printf "\e[0;33m")
set -x __YELLOW  (printf "\e[1;93m")
