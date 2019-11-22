" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

" Layout _______________________________________________________________________

" ........................................................................ Setup
let g:pad           = ['      ', '     ']  " statusline padding [inner, outer]
"                       123456    12345
let s:expanded      = 0                    " statusline state (0) dfm (1) expanded
let s:font_changed  = 0                    " redraw flag
let s:show          = 1                    " statusline (0) off (1) on

"  Distraction free modes ______________________________________________________

" .................................................................. Switch view
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

" ................................................................. Line numbers
" toggle line numbers
function! s:toggleNumber()
  Trace ui:ToggleNumber
  let g:duochrome_relative = ! g:duochrome_relative
  Background
endfunction

command! ToggleNumber silent! call <SID>toggleNumber()

" .................................................................. Insert mode
function! ToggleProof()
  Trace ui:ToggleProof()
  if CommandWindow() | return | endif
  " if Prose() | let g:duochrome_insert = ! g:duochrome_insert | endif
  let g:duochrome_insert = ! g:duochrome_insert
  call s:view()
endfunction

" Screen focus _________________________________________________________________

" ............................................................... Screen display
" initial view
function! LiteType()
  if PluginWindow() || ! has("gui_running") | return | endif 
  Trace ui:LiteType()
  let g:duochrome_markdown = Prose()
  call Font(Prose())
  call ScrollOffset()
  ColumnWrap
endfunction

" redraw
function! Retheme()
  if PluginWindow() | return | endif 
  Trace ui:Retheme()
  let lstatus     = &laststatus
  call LiteType()   
  let &laststatus = lstatus
endfunction

" .............................................................. Balance margins
function! Offset()
  return max([1, min([22, (&columns - &textwidth - 4) / 2])])  " account for linenr <space> text
endfunction

" balance left right margins with font size changes (and window resizing)
function! Margins()
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

" ..................................................................... Set font
" adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
function! Font(type)
  Trace ui:Font()
  if has('gui_running')
    if g:font_type != a:type
      let g:font_type = a:type
      let l:size      = system('fontsize')
      let l:size      = a:type == 0 ? l:size : l:size + g:font_step
      execute 'set guifont=' . (Prose() ? g:font[1] : g:font[0]) . ' ' . l:size
      if s:font_changed
        RedrawGui
      endif
      let s:font_changed = 1  " next font size change requires redraw
      set laststatus=2        " turn on statusline
    endif
  endif
endfunction

" Context statusline ___________________________________________________________

" ............................................................ Statusline format
function! Detail()
  let l:prefix = g:detail == 0 ? Tag() : Atom()
  return l:prefix > '' ? l:prefix . '  ' . SpecialChar() : SpecialChar()
endfunction

function! s:attn()
  return system('stat --printf %U ' . expand('%:p')) == 'root' ? '%3*' : '%1*'
endfunction

" [path] .. filename | pos .. [details]
function! s:statusline()
  " Trace ui:statusline()  " tmi :-)
  try  " trap snippet insertion interruption
    let g:prose = 1
    if Prose() && g:duochrome_insert
      return Escape(s:attn() . Leader('') . '  %{UnModified(0)}%1*')
    else
      let l:name     = '%{Name()}' . g:pad[0]
      if s:expanded == 0  " center dfm indicator / proofing statusline
        let l:leader = '%{Leader(Name())}'
      else
        let l:path   = '%{Path()}'
        let l:leader = '%{Leader(Path() . g:pad[1] . Name())}'
      endif
      let l:name     = '%1*' . l:name
      let l:info     = s:attn() . '%{UnModified(1)}' . g:pad[0] . '%1*%{PosWordsCol()}'  " utf-8 symbol occupies 2 chars (pad right 1 space)
      if s:expanded == 1
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
  ShowStatusLine
endfunction

command! ShowInfo silent! call <SID>showInfo()

function! s:toggleInfo(...)
  Trace ui:ToggleInfo
  if a:0 && a:1 | return | endif  " prose insert mode is always dfm
  let l:col = col('.')
  let s:expanded = ! s:expanded
  ShowInfo
  execute 'normal! ' . l:col . '|'
endfunction

command! -nargs=? ToggleInfo silent! call <SID>toggleInfo(<f-args>)

" ui.vim
