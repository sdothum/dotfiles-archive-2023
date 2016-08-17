
# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ................................................................ Session entry

# login check
if [ $USER != root ]
  if [ -z "$DISPLAY" -a (tty) = /dev/tty1 ]
    # assign deadline scheduler to SSD boot drive
    if [ (hostname) = luna -a ! -e /tmp/ssd:scheduler ]
      set SSD (ls -l /dev/disk/by-id/*ata* | grep 'Samsung_SSD' | head -1 | cut -d/ -f7)
      if [ $SSD ]
        # virtual block devices must be referenced explicitly
        switch $SSD
          case sda)  echo deadline | sudo tee /sys/block/sda/queue/scheduler >/dev/null
          case sdb)  echo deadline | sudo tee /sys/block/sdb/queue/scheduler >/dev/null
          case sdc)  echo deadline | sudo tee /sys/block/sdc/queue/scheduler >/dev/null
          case sdd)  echo deadline | sudo tee /sys/block/sdd/queue/scheduler >/dev/null
          case sde)  echo deadline | sudo tee /sys/block/sde/queue/scheduler >/dev/null
        esac
        notify "Assigned [deadline] scheduler to SSD" "/dev/$ssd"
        echo "/dev/$SSD [deadline]" > /tmp/ssd:scheduler
      end
    end
  end

  # set PATH so it includes user's private bin if it exists
  if [ -d "$HOME/bin" ]
    set -x PATH (echo $PATH | sed -e "s|$HOME/bin ||" -e "s|/opt/bin ||")
    set -x PATH "/opt/bin $PATH"
    for i in (find -L $HOME/bin -type d | grep -v '/\.' | sort -r)
      set -x PATH "$i $PATH" ^/dev/null
    end
  end

  # reset keyboard layout
  if [ -e /etc/vconsole.conf ]
    grep -q 'colemak' /etc/vconsole.conf
      and keymap qwerty
  end
end

# login triggers prezto fortune, insert blank line
if tty | grep -q '/dev/tty[1-9]'
  echo
else
  if [ -e /tmp/term:fortune ]
    if not [ -e /tmp/herbstluftwm:fortune 
      # su notify to apply user notification (spec)
      [ $USER = root ]
        and su -c "time=15 notify --urgency=critical \"$(fortune)\"" - shum ^/dev/null
        or time=15 notify "$(fortune)" ^/dev/null
      # touch /tmp/herbstluftwm:fortune
    end
    rm -f /tmp/term:fortune ^/dev/null
  end
end
