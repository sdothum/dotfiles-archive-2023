" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

" Layout _______________________________________________________________________

" ........................................................................ Setup
let s:expanded      = 0                    " statusline state (0) dfm (1) expanded
let s:font_changed  = 0                    " redraw flag

"  Distraction free modes ______________________________________________________

" .................................................................. Insert mode
" toggle full document highlight
function! s:view()
  Trace ui:view()
  let l:col = virtcol('.')
  if g:duochrome_insert  " insert dfm view
    Limelight
  else                   " normal proof view
    Limelight!
  endif
  execute 'normal! ' . l:col . '|'
  Background
endfunction

function! s:toggleProof()
  Trace ui:ToggleProof()
  if CommandWindow() | return | endif
  " if Prose() | let g:duochrome_insert = !g:duochrome_insert | endif
  let g:duochrome_insert = !g:duochrome_insert
  call s:view()
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

" Screen focus _________________________________________________________________

" ............................................................... Screen display
" initial view
function! s:layout()
  if PluginWindow() || !has("gui_running") | return | endif 
  Trace ui:Layout()
  let g:duochrome_markdown = Prose()
  Font Prose()
  ShowBreak
endfunction

command! Layout silent! call <SID>layout()

" refresh layout
function! s:refresh()
  if PluginWindow() | return | endif 
  Trace ui:Refresh()
  let lstatus     = &laststatus
  Layout   
  let &laststatus = lstatus
endfunction

command! Refresh silent! call <SID>refresh()

" .............................................................. Balance margins
function! Offset()
  return max([1, min([22, (&columns - &textwidth - 4) / 2])])  " account for linenr <space> text
endfunction

" balance left right margins with font size changes (and window resizing)
function! s:margins()
  Trace ui:Margin
  if PluginWindow()  " flush left for plugin windows
    setlocal nonumber
    setlocal foldcolumn=0
  else
    let g:lite_dfm_left_offset = Offset()
    Quietly LiteDFM
    ShowInfo
  endif 
endfunction

command! Margins silent! call <SID>margins()

" ..................................................................... Set font
" adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
function! s:font(type)
  Trace ui:Font()
  if has('gui_running')
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
  endif
endfunction

command! -nargs=1 Font silent! call <SID>font(<f-args>)

" Context statusline ___________________________________________________________

" ............................................................ Statusline format
function! Detail()
  let l:prefix = g:detail ? Atom() : Tag()
  return empty(l:prefix) ? SpecialChar() : l:prefix . '  ' . SpecialChar()
endfunction

function! s:attn()
  return system('stat --printf %U ' . expand('%:p')) == 'root' ? '%3*' : '%1*'
endfunction

" [path] .. filename | pos .. [details]
function! s:statusline()
  " Trace ui:statusline()  " tmi :-)
  try  " trap snippet insertion interruption
    if Prose() && g:duochrome_insert
      return Escape(s:attn() . Leader('') . '  %{UnModified(0)}%1*')
    else
      let l:name     = '%{Name()}' . g:pad[0]
      if s:expanded  " center dfm indicator / proofing statusline
        let l:path   = '%{Path()}'
        let l:leader = '%{Leader(Path() . g:pad[1] . Name())}'
      else
        let l:leader = '%{Leader(Name())}'
      endif
      let l:name     = '%1*' . l:name
      let l:info     = s:attn() . '%{UnModified(1)}' . g:pad[0] . '%1*%{PosWordsCol()}'  " utf-8 symbol occupies 2 chars (pad right 1 space)
      if s:expanded
        let l:name   = '%2*' . l:path . '%1*' . g:pad[1] . l:name
        let l:info  .= g:pad[1] . '%2*%{Detail()}'
      endif
      return Escape('%1*' . l:leader . l:name . l:info . '%1*')
    endif
  catch /.*/  " discard messages
  endtry
endfunction

" .............................................................. Show statusline
function! s:showInfo()
  Trace ui:showInfo()
  execute 'set statusline=' . s:statusline()
  StatusLine
endfunction

command! ShowInfo silent! call <SID>showInfo()

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
