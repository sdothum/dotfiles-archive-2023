" sdothum - 2016 (c) wtfpl

" Library
" ══════════════════════════════════════════════════════════════════════════════

" Vim __________________________________________________________________________

" ................................................................... Reload vim
" this function can only be defined in autoloaded source to avoid reload conflict
function! s:vimrc()
  execute 'wall'
  autocmd!
  source $MYVIMRC
  RedrawGui
endfunction

" when updates won't break the current vim session!
command! Vimrc call <SID>vimrc()

" .................................................................. Config file
" handy searchable lists
command! Hi  enew | put=execute('hi')  | normal gg
command! Map enew | put=execute('map') | normal gg

" System _______________________________________________________________________

" ........................................................... Error message trap
" ignore 1st time error messages from plugins (uninitialized s:variables)
function! s:quietly(command)
  try
    execute a:command
  catch /.*/  " discard messages
  endtry
endfunction

command! -nargs=1 Quietly call <SID>quietly(<f-args>)

" ........................................................ State change notifier
" nargs -> <message>: <bool>, note ':' separator terminating message
function! s:notify(s)
  let l:msg = split(a:s, ' *: ')  " accept g:varname in bool expression
  execute 'let l:state = ' . msg[1] . ' ? "ON" : "OFF"'
  echo l:msg[0] l:state
endfunction

command! -nargs=1 Notify call <SID>notify(<f-args>)

" ............................................................. GUI delay window
" wait for prior gui action to properly complete
function! s:waitFor(...)
  execute 'sleep ' . (a:0 ? a:1 : '10m')
endfunction

command! -nargs=? -bar WaitFor call <SID>waitFor(<f-args>)

" ................................................................ Cycle counter
function! TriToggle(counter, reset)
  if a:counter == 2 | return a:reset | endif
  return a:counter + 1
endfunction

" Text _________________________________________________________________________

" ............................................................. (Non-)blank line
function! NonBlankLine()
  return !empty(matchstr(getline(line('.')), '\S'))
endfunction

function! BlankLine()
  return !NonBlankLine()
endfunction

" ................................................................... Print file
" latex printing
function! s:hardcopy()
  echo 'Printing..'
  if Markdown()                 | execute '!hardcopy wiki \"' . expand('%:t') . '\"'
  elseif expand('%:e') =~ 'wps' | execute '!hardcopy wps' expand('%:t')
  else                          | execute '!hardcopy code' expand('%:t')
  endif
endfunction

command! Hardcopy silent call <SID>hardcopy()

" Filetype _____________________________________________________________________

" ............................................................. Prose filestypes
" distraction free filetyes
function! Prose()
  return &filetype =~ 'draft\|html\|mail\|markdown\|note\|wiki'
endfunction

function! Markdown()
  return &filetype =~ 'markdown\|wiki'
endfunction

" .................................................................... Protected
function! s:fzfBuffer()
  if exists("g:fzf#vim#buffers") | return g:fzf#vim#buffers != {} " fzf trap
  else                           | return 0
  endif
endfunction

function! Protected()
  return &filetype == 'help' || mode() == 't' || <SID>fzfBuffer()
endfunction

" ............................................................... Plugin windows
" plugin buffers typically are named '[<plugin>]' or '__<plugin>__'
function! PluginWindow()
  return expand('%:r') =~ '^[[_].*'
endfunction

function! CommandWindow()
  return expand('%p') == '[Command Line]'
endfunction

" lib.vim
