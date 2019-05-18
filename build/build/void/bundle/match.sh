#!/usr/bin/dash

# match.sh <missing packages> <repo list> -> <repo list>.found

filter='
  arch archers 
  base
  cli common core 
  en fonts* 
  git good gtk 
  html input 
  lib lua 
  mail manager modules 
  patched pdf perl php plugins* py 
  rss ruby 
  settings shell svn 
  term themes* tools ttf*
  ugly ui unicode url utils 
  wn 
  xf xft xorg
'

filter=$(echo $filter | sed 's/  */\\\|/g')
for i in $(cat $1) ;do
  for j in $(echo $i | tr '-' '\n') ;do 
    k=$(echo $j | sed 's/[0-9].*$//')
    [ $k ] || continue
    echo $k | grep -q "$filter" && continue
    grep -iq $k $2 && echo "$i -> $k :: $(grep -i $k $2 | tr '\n' ' ')"
  done
done >$2.found


