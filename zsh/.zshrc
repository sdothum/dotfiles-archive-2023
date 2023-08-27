
# Zsh Interactive Session Config
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# zmodload zsh/zprof

user_login

# ........................................................ Source plugin manager

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]] ;then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi
# source <(antibody init)
# antibody bundle <$HOME/.zsh/antibody.plugins 2>/dev/null

# ................................................................... zsh config

# auto-completion
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select

source ~/.zsh/setopts.zsh
source ~/.zsh/bindkeys.zsh
source ~/.zsh/aliases.zsh
source ~/.zsh/colors.zsh

# "prompt -s" doesn't work yet
autoload -Uz promptinit
promptinit 2>/dev/null && prompt shum >/dev/null
# if no prompt theme loaded
if [[ "$PROMPT" = '%m%# ' ]] ;then
  PROMPT='%F{red}    ─────  %f%b%F{blue}'
  RPROMPT='%F{yellow}%~%f  %F{green}%(?::%F{red})%D{%-I:%M %S}'
fi

if [[ -s /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]] ;then
  source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi
if [[ -s /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] ;then
  source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# history search
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search

# ........................................................... Autoload functions

# .zshenv is used to make functions available to dmenu system
autoload -Uz run-help
autoload -Uz run-help-git
autoload -Uz run-help-svn
autoload -Uz run-help-svk
alias help=run-help

# ................................................................... Deprecated

unset GREP_OPTIONS

# zprof
