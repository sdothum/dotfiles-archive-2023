
# Options
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ................................................................ Shell options

# correct commands.
setopt CORRECT

# auto-completion of command line switches for aliases
setopt COMPLETE_ALIASES

# disable bang history (exclamation! repeat)
unsetopt BANG_HIST

# start rbenv
# export PATH="$HOME/.rbenv/bin:$PATH"
# eval "$(rbenv init -)"

# add homebrew to the completion path
# fpath=("/usr/local/bin/" $fpath)

# why would you type 'cd dir' if you could just type 'dir'?
setopt AUTO_CD

# now we can pipe to multiple outputs!
setopt MULTIOS

# this makes cd=pushd
setopt AUTO_PUSHD

# this will use named dirs when possible
setopt AUTO_NAME_DIRS

# if we have a glob this will expand it
setopt GLOB_COMPLETE
setopt PUSHD_MINUS

# no more annoying pushd messages...
# setopt PUSHD_SILENT

# blank pushd goes to home
setopt PUSHD_TO_HOME

# this will ignore multiple directories for the stack.  Useful?  I dunno.
setopt PUSHD_IGNORE_DUPS

# 10 second wait if you do something that will delete everything.  I wish I'd had this before...
# setopt RM_STAR_WAIT

# use magic (this is default, but it can't hurt!)
setopt ZLE

setopt NO_HUP

# only fools wouldn't do this ;-)
# export EDITOR="subl -n -w"
export EDITOR="gvim -f"

# re-enable ctrl-d to end session!
# setopt IGNORE_EOF
# bindkey '^D' extended_logout

# if I could disable Ctrl-s completely I would!
setopt NO_FLOW_CONTROL

# keep echo "station" > station from clobbering station
# setopt NO_CLOBBER
unsetopt NO_CLOBBER

# case insensitive globbing
setopt NO_CASE_GLOB

# be reasonable!
setopt NUMERIC_GLOB_SORT

# i don't know why I never set this before.
setopt EXTENDED_GLOB
setopt GLOBDOTS

# hows about arrays be awesome?  (that is, frew${cool}frew has frew surrounding all the variables, not just first and last
setopt RC_EXPAND_PARAM
