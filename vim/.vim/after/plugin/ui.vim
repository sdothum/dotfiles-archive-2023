" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

" Layout _______________________________________________________________________

" ........................................................................ Setup
let g:pad           = ['      ', '   ']  " statusline padding [inner, outer]
"                       123456    123
let s:expanded      = 0                  " statusline state (0) dfm (1) expanded
let s:font_changed  = 0                  " redraw flag
let s:show          = 1                  " statusline (0) off (1) on

"  Distraction free modes ______________________________________________________

" .................................................................... Code view
" source code style
function! s:codeView()
  Trace ui:codeView()
  let g:duochrome_dfm      = 0
  let g:duochrome_markdown = 0
  " syntax enable  " restore CursorLine syntax highlighting before applying themes
  if exists('g:loaded_limelight')
    Limelight!
  endif
  Theme
  ShowStatusLine
  set showmode
endfunction

" ................................................................... Prose view
" distraction free style
function! s:proseView()
  Trace ui:proseView()
  let g:duochrome_dfm  = 1
  if Prose() | let g:duochrome_markdown = 1 | endif
  if Prose() || g:duochrome_ruler == 0 | set colorcolumn=0 | endif
  set foldcolumn=0
  set laststatus=0
  set noshowmode
  set scrolloff=8
  if Prose() | set spell
  else       | set nospell | endif
  call s:view()
endfunction

" .................................................................. Switch view
" toggle full document highlight
function! s:view()
  Trace ui:view()
  let l:col = virtcol('.')
  if g:duochrome_insert  " dfm view
    Contrast 1
    Limelight
  else                   " proof view
    Contrast 0
    Limelight!
  endif
  execute 'normal! ' . l:col . '|'
endfunction

function! s:setView()
  Trace ui:setView()
  if g:duochrome_dfm | call s:proseView()
  else               | call s:codeView() | endif
endfunction

" toggle dfm view
function! s:switchView()
  Trace ui:SwitchView
  let l:col = col('.')
  let g:duochrome_dfm = g:duochrome_dfm == 0 ? 1 : 0
  call s:setView()
  execute 'normal! ' . l:col . '|'
  Background
endfunction

command! SwitchView silent! call <SID>switchView()

" .................................................................. Insert mode
function! ToggleProof()
  Trace ui:ToggleProof()
  if CommandWindow() | return | endif
  " if Prose() | let g:duochrome_insert = g:duochrome_insert == 0 ? 1 : 0 | endif
  let g:duochrome_insert = g:duochrome_insert == 0 ? 1 : 0
  call s:view()
  ShowInfo
endfunction

" Screen focus _________________________________________________________________

" ............................................................... Screen display
" initial view
function! LiteType()
  if PluginWindow() || ! has("gui_running") | return | endif 
  Trace ui:LiteType()
  call Font(Prose())
  call s:setView()
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
  let s:expanded = (s:expanded == 0 ? 1 : 0)
  ShowInfo
  execute 'normal! ' . l:col . '|'
endfunction

command! -nargs=? ToggleInfo silent! call <SID>toggleInfo(<f-args>)

" ui.vim
