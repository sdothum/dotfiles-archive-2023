" sdothum - 2016 (c) wtfpl

" GUI
" ══════════════════════════════════════════════════════════════════════════════

" Highlights ___________________________________________________________________

" ......................................................... Trailing white space
augroup invisible | autocmd! | augroup END

" toggle trailing whitespace highlight
function! s:toggleWhiteSpace()
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

command! ToggleWhiteSpace call <SID>toggleWhiteSpace()

" Search and replace ___________________________________________________________

" ........................................................... Incremental search
let s:separator = ' '  " line wrap enabled incsearch (including irregular spacing)

function! s:toggleWrapSearch()
  let s:separator = s:separator == ' ' ? '\_s*' : ' '
  cnoremap <expr><space>  '/?' =~ getcmdtype() ? s:separator : ' '
  Notify Wrap search: s:separator != ' '
endfunction

command! ToggleWrapSearch call <SID>toggleWrapSearch()

" ...................................................................... Replace
" restore search highlight after replace
function! s:searchReplace(cmd)
  let l:search = @/
  let l:s = input('', a:cmd)
  execute l:s
  let @/ = l:search
endfunction

command! -nargs=1 SearchReplace silent! call <SID>searchReplace(<f-args>)

" gui.vim
