# sdothum - 2016   (c) wtfpl

# Fish Shell
# ══════════════════════════════════════════════════════════════════════════════

# ........................................................ ANSI terminal colours

# for posix shell scripts
set -x BLINK         (printf "\e[5m")
set -x FAINT         (printf "\e[2m")
set -x ITALIC        (printf "\e[3m")
set -x REVERSE       (printf "\e[7m")
set -x STRIKETHROUGH (printf "\e[9m")
set -x UNDERLINE     (printf "\e[4m")

set -x NOBLINK       (printf "\e[25m")
set -x NOCOLOR       (printf "\e[0;0m")

# normal
# BRIGHT
set -x black         (printf "\e[0;30m")
set -x BLACK         (printf "\e[1;90m")
set -x white         (printf "\e[0;37m")
set -x WHITE         (printf "\e[1;97m")
set -x blue          (printf "\e[0;34m")
# set -x BLUE        (printf "\e[1;94m")
set -x BLUE          (printf "\e[1m\e[38;5;075m")
set -x cyan          (printf "\e[0;36m")
# set -x CYAN        (printf "\e[1;96m")
set -x CYAN          (printf "\e[1m\e[38;5;051m")
set -x green         (printf "\e[0;32m")
# set -x GREEN       (printf "\e[1;92m")
set -x GREEN         (printf "\e[1m\e[38;5;046m")
set -x magenta       (printf "\e[0;35m")
# set -x MAGENTA     (printf "\e[1;95m")
set -x MAGENTA       (printf "\e[1m\e[38;5;201m")
set -x red           (printf "\e[0;31m")
# set -x RED         (printf "\e[1;91m")
set -x RED           (printf "\e[1m\e[38;5;196m")
set -x yellow        (printf "\e[0;33m")
# set -x YELLOW      (printf "\e[1;93m")
set -x YELLOW        (printf "\e[1m\e[38;5;226m")

# 256 color palette, see ditto
set -x turquoise     (printf "\e[38;5;031m")
set -x TURQUOISE     (printf "\e[1m\e[38;5;039m")

set -x brown         (printf "\e[38;5;215m")
set -x BROWN         (printf "\e[1m\e[38;5;221m")
set -x orange        (printf "\e[38;5;208m")
set -x ORANGE        (printf "\e[1m\e[38;5;214m")
set -x pink          (printf "\e[38;5;206m")
set -x PINK          (printf "\e[1m\e[38;5;212m")
set -x teal          (printf "\e[38;5;023m")
set -x TEAL          (printf "\e[1m\e[38;5;029m")
