# Zsh Interactive Session Config
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# zmodload zsh/zprof

# ................................................................ Source prezto

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]] ;then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# ................................................................ Session entry

# login check
if [[ $USER != root ]] ;then
  if [[ -z "$DISPLAY" && $(tty) = /dev/tty1 ]] ;then
    # assign deadline scheduler to SSD boot drive
    if [[ $(hostname) = luna && ! -e /tmp/ssd:scheduler ]] ;then
      ssd=$(ls -l /dev/disk/by-id/*ata* | grep 'Samsung_SSD' | head -1 | cut -d/ -f7)
      if [[ $ssd ]] ;then
        # virtual block devices must be referenced explicitly
        case $ssd in
          sda)  echo deadline | sudo tee /sys/block/sda/queue/scheduler >/dev/null ;;
          sdb)  echo deadline | sudo tee /sys/block/sdb/queue/scheduler >/dev/null ;;
          sdc)  echo deadline | sudo tee /sys/block/sdc/queue/scheduler >/dev/null ;;
          sdd)  echo deadline | sudo tee /sys/block/sdd/queue/scheduler >/dev/null ;;
          sde)  echo deadline | sudo tee /sys/block/sde/queue/scheduler >/dev/null ;;
        esac
        notify "Assigned [deadline] scheduler to SSD" "/dev/$ssd"
        echo "/dev/$ssd [deadline]" > /tmp/ssd:scheduler
      fi
    fi
  fi

  # reset keyboard layout
  if [[ -e /etc/vconsole.conf ]] ;then
    grep -q 'colemak' /etc/vconsole.conf && keymap qwerty
  fi
fi

# "prompt -s" doesn't work yet
autoload -Uz promptinit
promptinit 2>/dev/null && prompt shum
# login triggers prezto fortune, insert blank line
if [[ $(tty) =~ /dev/tty[1-9] ]] ;then
  echo
else
  if ! [ -e /tmp/herbstluftwm:fortune ] ;then
    # su notify to apply user notification (spec)
    [[ $USER = root ]] && su -c "time=15 notify --urgency=critical \"$(fortune)\"" - shum 2>/dev/null \
                       || time=15 notify "$(fortune)" 2>/dev/null
    # touch /tmp/herbstluftwm:fortune
  fi
fi

# ................................................................... zsh config

source ~/.zsh/setopts.zsh
source ~/.zsh/bindkeys.zsh
source ~/.zsh/aliases.zsh
# source ~/.zsh/colors.zsh
if [[ -s /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]] ;then
  source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi 

# ........................................................... Autoload functions

# .zshenv is used to make functions available to dmenu system
autoload run-help-git
autoload run-help-svn
autoload run-help-svk

# ................................................................... Deprecated

unset GREP_OPTIONS

# zprof
