#!/usr/bin/fish
# todo loop for tmux/screen window
#
#
# todo actions are all bash scripts and would require conversion to 
#
#

# force (no prompt), timestamp, verbose
set options -f -t -v $argv

function title
  printf '%s[ TODO.txt ]%s%s%s' (set_color -b blue white) (head -c (echo (tput cols) - 12 | bc) < /dev/zero | tr '\0' ' ') (set_color normal) (set_color white)
end

function header; clear; title; echo :$argv; end
function refresh; header $list; do_todo $list; end

function prompt
  printf '%s%s %s-%s-  %s!todo ' (set_color green) (date '+%A, %d %b %Y') (set_color red) (date '+%-I:%M%P') (set_color white)
end

function do_todo
  # if filter is anything other than "on", disable post filtering
  echo todo $options (echo $filter | sed -e 's/^on//' -e 's/^..*/-x/') $argv >>~/tmp/todo.log
  todo $options (echo $filter | sed -e 's/^on//' -e 's/^..*/-x/') $argv 2>>~/tmp/todo.log
end

set list list; set filter on; refresh
read
while true
  echo; printf "  %s{ set list=$list   filter=$filter   project=$project   context=$context   priority=$priority }%s" (set_color -o black) (set_color normal)
  read -m vi -p prompt action
  if test "$action" = "q" ; break; else; set refresh true; end
  if test "$action" = "" ; set action $list; end
  if test (echo "$action" | egrep '^(list|ls|projectview|pv|contextview|cv)'); set refresh false; header $action; end 
  # set some handy "add" and "list" defaults
  # set list=action e.g. pv, ls +project, ls @context, ls string
  # set project=+name
  # set context=@attribute
  # set priority=letter
  # to clear a setting...
  #   set variable=
  if test (echo "$action" | egrep '^set '); set (echo $action | sed -e 's/^set //'); continue; end
  if test (echo "$priority" | egrep '^[A-Za-z]'); set pri pri $priority; else; set pri; end
  if test (echo "$action" | egrep '^a '); set action $action $project $context; end
  if test (echo "$action" | egrep '^add '); set action add $pri (echo $action | sed -e 's/^add //') $project $context; end
  # strip out extraneous spaces
  set action (echo $action | sed -e 's/  */ /g' -e 's/ $//g')
  do_todo $action
  if test (echo $action | egrep '^(edit|help)'); refresh; end
end
if test $refresh = true; refresh; end
#echo; echo "  \"todo -x\" to disable filter"
echo
