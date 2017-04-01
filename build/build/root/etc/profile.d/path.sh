# /etc/profile.d/path.sh
# see .xinitrc -> . /etc/profile
[ $USER = root ] && exit

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
  PATH=$(echo $PATH | sed -e "s|$HOME/bin:||" -e "s|$HOME/.local/bin:||" -e 's|/opt/bin:||')
  PATH="$HOME/.local/bin:/opt/bin:$PATH"
  for i in $(find -L $HOME/bin -type d | grep -v '/\.' | sort -r)
  do
    PATH="${i}:$PATH"
  done
fi
