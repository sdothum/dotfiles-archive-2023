" sdothum - 2016 (c) wtfpppl

" GUI
" ══════════════════════════════════════════════════════════════════════════════

" Behaviour ____________________________________________________________________

" ................................................................... Toggle gui
" toggle gui menu
function! s:toggleGui()
  if &guioptions =~# 'm' | set guioptions-=m
  else                   | set guioptions+=m | endif
endfunction

command! ToggleGui silent! call <SID>toggleGui()

" ................................................................... Redraw gui
let s:delay = '200m'  " redraw delay, see ui:Font()

" toggle in/out to fill window
function! s:redrawGui()
  ToggleGui
  execute 'sleep ' . s:delay
  ToggleGui
  if g:wrap_highlighting
    Quietly Retheme  " fix line wrap highlighting
  endif
endfunction

command! RedrawGui silent! call <SID>redrawGui()

" .................................................................... Scrolling
" dynamic scroll offset
function! s:scrollOffset()
  let &scrolloff = Prose() ? 999 : 3
endfunction

command! ScrollOffset silent! call <SID>scrollOffset()

" Look _________________________________________________________________________

" ............................................................... Column margins
augroup column | autocmd! | augroup END

set colorcolumn=0  " highlight column

" toggle colorcolumn modes, see theme:Guides()
function! s:toggleColumn()
  if g:duochrome_ruler == 0
    let g:duochrome_ruler = 1
    let &colorcolumn = col('.') 
    autocmd column CursorMoved,CursorMovedI * let &colorcolumn = col('.')
  elseif g:duochrome_ruler == 1
    let g:duochrome_ruler = 2
    autocmd! column
  else
    let g:duochrome_ruler = 0
    let &colorcolumn      = 0
    ColumnWrap
  endif
  let g:show_column       = 1  " flash column position, see autocmd info.vim
  Background
endfunction

command! ToggleColumn silent! call <SID>toggleColumn()

" Highlights ___________________________________________________________________

" .......................................................... Line wrap highlight
let s:breakchar = '\ ↪\ '  " \escape spaces

" highlight wrapped line portion, see theme:Theme()
function! s:columnWrap()
  if g:duochrome_ruler == 0 && s:wraplight
    set showbreak=
    let l:edge       = winwidth(0) - &numberwidth - &foldcolumn - 1
    let &colorcolumn = join(range(l:edge, 999), ',')
  else
    execute 'set showbreak=' . s:breakchar
  endif
endfunction

command! ColumnWrap silent! call <SID>columnWrap()

let s:wraplight = 0  " show linewrap with (0) s:breakchar (1) highlight

function! s:toggleColumnWrap(...)
  let s:wraplight       = a:0 ? a:1 : !s:wraplight
  let g:duochrome_ruler = -1
  ToggleColumn
endfunction

command! -nargs=? ToggleColumnWrap silent! call <SID>toggleColumnWrap(<f-args>)

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
    autocmd invisible BufLeave,WinLeave * call <SID>toggleWhiteSpace()
  endif
  Status List invisibles: &list != ' '
endfunction

command! ToggleWhiteSpace call <SID>toggleWhiteSpace()

" gui.vim
