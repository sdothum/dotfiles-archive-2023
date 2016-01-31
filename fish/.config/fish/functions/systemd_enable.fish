function systemd_enable
  sudo systemctl enable $argv
  sudo systemctl start $argv
  sudo systemctl status $argv
end
