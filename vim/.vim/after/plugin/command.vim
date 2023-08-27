" sdothum - 2016 (c) wtfpl

" Library
" ══════════════════════════════════════════════════════════════════════════════

" Vim __________________________________________________________________________

" ................................................................... Reload vim
" this function can only be defined in autoloaded source to avoid reload conflict
function! s:vimrc()
  execute 'wall'
  autocmd!
  unlet g:loaded_duochrome  " re-initialize font settings
  source $MYVIMRC
  Layout
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
  let l:type = Markdown() ? 'wiki' : (expand('%:e') =~ 'wps' ? 'wps' : 'code')
  call system('hardcopy ' . l:type . ' "' . expand('%:t') . '"')
endfunction

command! Hardcopy silent call <SID>hardcopy()

" command.vim
