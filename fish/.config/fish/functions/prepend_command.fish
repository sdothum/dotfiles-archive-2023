function prepend_command --description 'sudo history expansion'
  # lifted from https://github.com/fish-shell/fish-shell/issues/288#issuecomment-158704275
  # see fish_user_key_bindings
  set -l prepend $argv[1]
  if test -z "$prepend"
    echo "prepend_command needs one argument."
    return 1
  end

  set -l cmd (commandline)
  if test -z "$cmd"
    commandline -r $history[1]
  end

  set -l old_cursor (commandline -C)
  commandline -C 0
  commandline -i "$prepend "
  commandline -C (math $old_cursor + (echo $prepend | wc -c))
end
