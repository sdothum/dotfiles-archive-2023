" sdothum - 2016 (c) wtfpl

" GUI
" ══════════════════════════════════════════════════════════════════════════════

" Highlights ___________________________________________________________________

" ......................................................... Trailing white space
augroup invisible | autocmd! | augroup END

" toggle trailing whitespace highlight
function! gui#ToggleWhiteSpace()
  set list!
  if &list == 0
    match ExtraWhitespace /\%x00$/  " nolist by failing match with null character :)
    autocmd! invisible
  else
    match ExtraWhitespace /\s\+$/
    " list state propagates forward (on) but not backwards (off)? so auto reset buffer state!
    autocmd invisible BufLeave,WinLeave * ToggleWhiteSpace
  endif
  Notify List invisibles: &list != ' '
endfunction

" Search and replace ___________________________________________________________

" ........................................................... Incremental search
let g:incseparator = ' '  " line wrap enabled incsearch (including irregular spacing)

function! gui#ToggleWrapSearch()
  let g:incseparator = g:incseparator == ' ' ? '\_s*' : ' '
  cnoremap <expr><space>  '/?' =~ getcmdtype() ? g:incseparator : ' '
  Notify Wrap search: g:incseparator != ' '
endfunction

" ...................................................................... Replace
" restore search highlight after replace
function! gui#SearchReplace(cmd)
  let l:search = @/
  let l:s = input('', a:cmd)
  execute l:s
  let @/ = l:search
endfunction

" gui.vim
