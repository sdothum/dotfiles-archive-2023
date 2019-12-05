" sdothum - 2016 (c) wtfpl

" Theme
" ══════════════════════════════════════════════════════════════════════════════

" Colorscheme __________________________________________________________________

" ............................................................... Initialization
function! s:background(...)
  Trace theme:Background
  Margins  " must be called before colorscheme ta refresh cursorlinenr cleared by LiteDFM
  if a:0 | let &background = a:1
  else   | execute 'set background=' . &background
  endif
endfunction

command! -nargs=? -bar Background silent! call <SID>background(<f-args>)

" .......................................................... Toggle colourscheme
function! s:liteSwitch()
  Trace theme:LiteSwitch
  Quietly LiteDFMClose  " trap and ignore initialization error
  if &background == 'light' | Background dark
  else                      | Background light
  endif
endfunction

command! LiteSwitch silent! call <SID>liteSwitch()

" Statusline ___________________________________________________________________

" ................................................................ Single window
function! s:statusLine()
  Trace theme:StatusLine
  set laststatus=2
endfunction

command! StatusLine silent! call <SID>statusLine()

" ................................................................ Split windows
" split window statusline background, see ui.vim autocmd
function! s:splitColors()
  Trace theme:SplitColors
  let g:duochrome_split = &diff ? 0 : winnr('$') > 1
  Background
endfunction

command! -bar SplitColors silent! call <SID>splitColors()

" theme.vim
