function man
  env \
    LESS_TERMCAP_mb=(printf "\033[1;31m") \
    LESS_TERMCAP_md=(printf "\033[1;31m") \
    LESS_TERMCAP_me=(printf "\033[0m") \
    LESS_TERMCAP_se=(printf "\033[0m") \
    LESS_TERMCAP_so=(printf "\033[1;44;33m") \
    LESS_TERMCAP_ue=(printf "\033[0m") \
    LESS_TERMCAP_us=(printf "\033[1;32m") \
      man "$argv"
end
