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

" ..................................................................... Terminal
" !term fails on shell error 1 (?)
command! Term :call system('term "vimterm" STACK')

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

" Text _________________________________________________________________________

" ............................................................. (Non-)blank line
function! NonBlankLine()
  return !empty(matchstr(getline(line('.')), '\S'))
endfunction

function! BlankLine()
  return !NonBlankLine()
endfunction

" lib.vim
