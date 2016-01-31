function fish_prompt --description 'Write out the prompt'
  and set -g __returncode blue; or set -g __returncode red
  printf '%s%s %s%s %s%s %s:%s%s%s%s ' (set_color $__returncode) (date '+%a') (set_color cyan) (date '+%-I:%M %S%P') (set_color green) (whoami) (hostname|cut -d . -f1) (set_color green) (set_color yellow) (prompt_pwd) (set_color normal)
  set -e __returncode
end
