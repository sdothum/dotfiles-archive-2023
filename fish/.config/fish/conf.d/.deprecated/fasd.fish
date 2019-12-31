# sdothum - 2016 (c) wtfpl

# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ......................................................................... fasd

# modified from https://github.com/oh-my-fish/plugin-fasd
 
function __fasd_run -e fish_preexec
  command fasd --proc (command fasd --sanitize "$argv") > "/dev/null" 2>&1 &
end

function __fasd_print_completions
  set cmd (commandline -po)
  fasd $argv $cmd[2..-1] -l
end

function z
  cd (fasd -d -e 'printf %s' "$argv") >/dev/null
end

complete -c z -a "(__fasd_print_completions -d)" -f -A
