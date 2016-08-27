# sdothum - 2016 (c) wtfpl

# Fish Shell
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ........................................................................ chips

set -l chips ~/.config/chips/dist

set function_path $chips/*/functions
set complete_path $chips/*/completions

# autoload functions
test -n "$function_path"
  and set fish_function_path $fish_function_path[1] \
                             $function_path \
                             $fish_function_path[2..-1]

# autoload completions
test -n "$complete_path"
  and set fish_complete_path $fish_complete_path[1] \
                             $complete_path \
                             $fish_complete_path[2..-1]

# Backup key bindings
functions -q fish_user_key_bindings
  and not functions -q __original_fish_user_key_bindings
  and functions -c fish_user_key_bindings __original_fish_user_key_bindings
# Override key bindings, calling original if existent
function fish_user_key_bindings
  # Prepare packages key bindings paths
  set -l key_bindings $chips/*/key_binding?.fish
  # Source all keybindings collected
  for file in $key_bindings
    source $file
  end
  # Call original key bindings if existent
  functions -q __original_fish_user_key_bindings
    and __original_fish_user_key_bindings
end
