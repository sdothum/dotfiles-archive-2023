" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

" Distraction free mode ________________________________________________________

" .................................................................. Insert mode
" toggle full document highlight
function! s:toggleProof()
  Trace ui:ToggleProof()
  if CommandWindow() | return | endif
  let l:col              = virtcol('.')
  let g:duochrome_insert = !g:duochrome_insert
  if g:duochrome_insert | Limelight           " insert mode dfm view
  else                  | Limelight! | endif  " normal mode proof view
  execute 'normal! ' . l:col . '|'
  Background
endfunction

command! -bar ToggleProof silent! call <SID>toggleProof()

" ................................................................. Line numbers
" toggle line numbers
function! s:toggleNumber()
  Trace ui:ToggleNumber
  let g:duochrome_relative = !g:duochrome_relative
  Background
endfunction

command! ToggleNumber silent! call <SID>toggleNumber()

" Screen _______________________________________________________________________

" ............................................................... Screen display
" initial view
function! s:layout()
  Trace ui:Layout()
  if PluginWindow() || !has("gui_running") | return | endif 
  let g:duochrome_markdown = Prose()
  Font Prose()
  ShowBreak
endfunction

command! Layout silent! call <SID>layout()

" refresh layout
function! s:refresh()
  Trace ui:Refresh()
  if PluginWindow() | return | endif 
  let l:status     = &laststatus
  Layout   
  let &laststatus = l:status
endfunction

command! Refresh silent! call <SID>refresh()

" .............................................................. Balance margins
" balance left right margins with font size changes (and window resizing)
function! s:margins()
  Trace ui:Margin
  if PluginWindow()  " flush left for plugin windows
    setlocal nonumber
    setlocal foldcolumn=0
  else
    let g:lite_dfm_left_offset = max([1, min([22, (&columns - &textwidth - 4) / 2])])  " account for linenr <space> text
    Quietly LiteDFM
    ShowInfo
  endif 
endfunction

command! Margins silent! call <SID>margins()

" ..................................................................... Set font
let s:font_changed = 0  " redraw flag

" adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
function! s:font(type)
  Trace ui:Font()
  if !has('gui_running') | return | endif 
  " a:type may be function expression (nargs are literal strings only)
  execute 'let l:type = ' . a:type
  if g:font_type != l:type
    let g:font_type = l:type
    let l:size      = system('fontsize')
    let l:size      = l:type ? l:size + g:font_step : l:size
    execute 'set guifont=' . (Prose() ? g:font[1] : g:font[0]) . ' ' . l:size
    if s:font_changed | RedrawGui | endif
    let s:font_changed = 1  " next font size change requires redraw
    set laststatus=2        " turn on statusline
  endif
endfunction

command! -nargs=1 Font silent! call <SID>font(<f-args>)

" Statusline ___________________________________________________________________

" .............................................................. Show statusline
let s:expanded = 0  " statusline state (0) dfm (1) expanded

function! s:showInfo()
  Trace ui:ShowInfo()
  execute 'set statusline=' . Statusline(s:expanded)
  StatusLine
endfunction

command! ShowInfo silent! call <SID>showInfo()

" ............................................................ Toggle statusline
function! s:toggleInfo(...)
  Trace ui:ToggleInfo
  if a:0 && a:1 | return | endif  " prose insert mode is always dfm
  let l:col = col('.')
  let s:expanded = !s:expanded
  ShowInfo
  execute 'normal! ' . l:col . '|'
endfunction

command! -nargs=? -bar ToggleInfo silent! call <SID>toggleInfo(<f-args>)

" ui.vim
