
# Logging functions
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ${_colors} defined in .zshenv 

# ................................................................... Annotation

# Initial annotation
function annotate {
  [[ -z "$annotate" ]] && annotate='.'
  indent=$(echo $(tput cols) - $(echo $@ | wc -L) - 3 | bc)
  [[ ( $indent -gt 0 ) ]] && leader=$(head -c $indent </dev/zero | tr '\0' "$annotate")
  log "${_CYAN}@ $leader $@${_nocolor}"
  annotate=
}

# Continuation line
function annotate+ {
  export annotate=' '
  annotate $@
}

# .................................................................... Attention

function drawline {
  [[ ( $# -eq 0 ) ]] && linechar='-' || linechar=$1
  echo ${_red}$(head -c $(tput cols) < /dev/zero | tr '\0' "$linechar")
}

function attention {
  log ${_YELLOW}$argv${_nocolor}
}

function underline {
  drawline '~'
  attention $@
}

# ...................................................................Sub-heading

function heading {
  if [[ ( $# -eq 1 ) || ( $(expr length $1) -gt 1 ) ]]; then
    char=':'
  else
    char=$1
    shift
  fi
  message=$@
  leader=$(head -c $(echo '('$(tput cols) - $(echo "   $message   " | wc -L)')' / 2 | bc) </dev/zero | tr '\0' "$char")
  [[ ( $(echo "$leader   $message   $leader" | wc -L) -lt $(tput cols) ) ]] && pad=$char
  attention "$leader   $message   $leader$pad"
}

# Time stamp heading
function heading+ {
  echo
  heading $@
  uname -r | grep -q 'ARCH\|ck' \
    && echo ${_GREEN}$(date '+@ %a %-I:%M %S%P')${_nocolor} \
    || echo
}

# Conditional heading
function heading_if {
	eval $2 && heading $1
}

# ................................................................ Section title

function title {
  echo
  drawline '='
  heading ' ' $@
  drawline '='
}

# Conditional title
function title_if {
	eval $2 && title $1; 
}

# .......................................................................... Log

function log {
  [[ -z $logfile ]] && export logfile=~/logs/session.log
  # use -n option to suppress line break
  echo $@ >> $logfile
  echo $@
}

# ........................................................................ Trace

function trace {
  # set or touch ~/.tracelevel to create session persistant tracelevel
  # "trace [0-9]" set global tracelevel, defaults to ~/.tracelevel, else 0
  # "trace [1-9] message"
  # "trace message" == "trace 0 message" which always echos
  # "trace" show current tracelevel
  if [[ -z $tracelevel ]]; then
    [[ -s ~/.tracelevel ]] && export tracelevel=$(cat ~/.tracelevel) || export tracelevel=0
  fi
  [[ ( $# -eq 0 ) ]] && echo tracelevel := $tracelevel && return
  if (( $(echo $1 | grep '^[0-9]$') )); then
    if [[ ( $# -eq 1 ) ]]; then
      export tracelevel=$@
      [[ -f ~/.tracelevel ]] && echo $@ > ~/.tracelevel
      trace
      return
    fi
    level=$1
    shift
    message=$@
  else
    level=0
    message=${_white}$@
  fi
  if [[ ( $level -le $tracelevel ) ]]; then
    log -n ${_green}$(date '+@ %a %-I:%M %S%P ')
    [[ ( $level -gt 0 ) ]] && log -n "${_RED}$level> "
    # message passed may contain set_color commands, so reset at end
    log "${_nocolor}$message${_nocolor}"
  fi
}
