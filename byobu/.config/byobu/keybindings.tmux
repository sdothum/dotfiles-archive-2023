# keybindings
bind-key -n F2 new-window -c "#{pane_current_path}" \; rename-window ""
bind-key -n F10 copy-mode

unbind-key -n C-a
set -g prefix ^A
set -g prefix2 ^A
bind a send-prefix
