function gitstatus
  set -g GIT_BIN        $HOME/bin/functions
  set -g GIT_EXECUTABLE python

  # Default values for the appearance of the prompt. Configure at will.
  set -g THEME_PREFIX      '('
  set -g THEME_SUFFIX      ')'
  set -g THEME_SEPARATOR   '|'
  set -g THEME_BRANCH      (set_color -o magenta)
  set -g THEME_STAGED      (set_color red)'●'
  set -g THEME_CONFLICTS   (set_color red)'✖'
  # set -g THEME_CHANGED   (set_color blue)'✚'
  set -g THEME_CHANGED     (set_color blue)'✱'
  set -g THEME_BEHIND      '↓'
  set -g THEME_AHEAD       '↑'
  # set -g THEME_UNTRACKED '…'
  set -g THEME_UNTRACKED   '◼'
  set -g THEME_CLEAN       (set_color -o green)'✔'

  function precmd_update_git_vars
    set -e GIT_STATUS
    ls .git >/dev/null ^&1
      and update_current_git_vars
  end

  function update_current_git_vars
    switch $GIT_EXECUTABLE
      case python
        set -g GIT_STATUS (python $GIT_BIN/gitstatus.py ^/dev/null  | tr ' ' \n)
      case haskell
        set -g GIT_STATUS (git status --porcelain --branch ^/dev/null | $GIT_BIN/gitstatus  | tr ' ' \n)
    end
    # echo $GIT_STATUS

    if [ -n "$GIT_STATUS" ]
      set -g GIT_BRANCH    $GIT_STATUS[1]
      set -g GIT_AHEAD     $GIT_STATUS[2]
      set -g GIT_BEHIND    $GIT_STATUS[3]
      set -g GIT_STAGED    $GIT_STATUS[4]
      set -g GIT_CONFLICTS $GIT_STATUS[5]
      set -g GIT_CHANGED   $GIT_STATUS[6]
      set -g GIT_UNTRACKED $GIT_STATUS[7]
      # echo $GIT_BRANCH $GIT_AHEAD $GIT_BEHIND $GIT_STAGED $GIT_CONFLICTS $GIT_CHANGED $GIT_UNTRACKED
    end
  end


  # update_current_git_vars
  precmd_update_git_vars
  if [ -n "$GIT_STATUS" ]
    set STATUS "$THEME_PREFIX$THEME_BRANCH$GIT_BRANCH"(set_color normal)
    if [ "$GIT_BEHIND" -ne 0 ]
      set STATUS "$STATUS$THEME_BEHIND$GIT_BEHIND"(set_color normal)
    end
    if [ "$GIT_AHEAD" -ne 0 ]
      set STATUS "$STATUS$THEME_AHEAD$GIT_AHEAD"(set_color normal)
    end
    set STATUS "$STATUS$THEME_SEPARATOR"
    if [ "$GIT_STAGED" -ne 0 ]
      set STATUS "$STATUS$THEME_STAGED$GIT_STAGED"(set_color normal)
    end
    if [ "$GIT_CONFLICTS" -ne 0 ]
      set STATUS "$STATUS$THEME_CONFLICTS$GIT_CONFLICTS"(set_color normal)
    end
    if [ "$GIT_CHANGED" -ne 0 ]
      set STATUS "$STATUS$THEME_CHANGED$GIT_CHANGED"(set_color normal)
    end
    if [ "$GIT_UNTRACKED" -ne 0 ]
      set STATUS "$STATUS$THEME_UNTRACKED"(set_color normal)
    end
    if [ "$GIT_CHANGED" -eq 0 ]
    and [ "$GIT_CONFLICTS" -eq 0 ]
    and [ "$GIT_STAGED" -eq 0 ]
    and [ "$GIT_UNTRACKED" -eq 0 ]
      set STATUS "$STATUS$THEME_CLEAN"
    end
    set STATUS "$STATUS"(set_color normal)"$THEME_SUFFIX"
    printf "$STATUS"
  end
end
