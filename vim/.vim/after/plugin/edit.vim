" sdothum - 2016 (c) wtfpl

" Edit
" ══════════════════════════════════════════════════════════════════════════════

" Line _________________________________________________________________________

" .................................................................. Insert line
" insert line while disabling auto-commenting OR break (prose) line
function! edit#SmartWrap()
  if Prose()  " override Pencil mode (the default state for prose)
    set paste
    execute " normal! i\<CR>"
    set nopaste
    execute 'startinsert'
  else        " append EOL wrap from any col position
    let l:formatoptions = &formatoptions  " disable auto commenting
    set formatoptions-=c
    set formatoptions-=r
    set formatoptions-=o
    normal! ^
    let l:pos = col('.')
    normal! o
    " align line indentation
    execute 'normal! a' . repeat(' ', l:pos)
    let &formatoptions = l:formatoptions
  endif
endfunction

" ............................................................. Strip whitespace
" strips trailing whitespace from all lines
function! edit#StripTrailingWhitespaces()
  if !&modifiable || Markdown() | return | endif
  " let l:_s = @/       " save last search & cursor position
  " let l:l  = line('.')
  " let l:c  = col('.')
  let s:view = winsaveview()
  %s/\s\+$//e           " EOL
  %s/\(\n\r\?\)\+\%$//e " EOF
  call winrestview(s:view)
  " let @/ = l:_s
  " call cursor(l:l, l:c)
endfunction

" Convert text _________________________________________________________________

" ......................................................... Paragraph formatting
function! edit#Inject(commands)
  if Prose() | execute 'normal! ' . a:commands | endif
endfunction

" .............................................................. Code block text
" convert wiki text lines into code block lines
function! edit#CodeBlock()
  execute "silent! normal  :s/\\(.*\\)/`\\1`/\<CR>"
  " preserve leading spaces with wiki markdown
  execute "silent! normal! gv:s/^` /`^ /\<CR>"
  execute "silent! normal! gv:s/^``/`^ `/e\<CR>"
  " convert [[test]], see thedarnedestthing markdown
  execute "silent! normal! gv:s/ \\[\\[ / [[] /e\<CR>"
  execute "silent! normal! gv:s/ \\]\\] / []] /e\<CR>"
endfunction

" Text shift ___________________________________________________________________

" .................................................................. Select text
function! s:nonBlankLine()
  return !empty(matchstr(getline(line('.')), '\S'))
endfunction

function! s:blankLine()
  return !s:nonBlankLine()
endfunction

function! edit#ParagraphAbove()
  if s:nonBlankLine()
    normal! {
    if s:blankLine()
      normal! j
    endif
  endif
  normal! }kV{
endfunction

function! edit#ParagraphBelow()
  if s:nonBlankLine()
    normal! }
    if s:blankLine()
      normal! k
    endif
  endif
  normal! {jV}
endfunction

" ................................................................ Shift up down
" move by lines
function! s:moveLineOrVisualUpOrDown(move)
  let l:col = virtcol('.')
  execute 'silent! ' . a:move
  execute 'normal! ' . l:col . '|'
endfunction

function! s:moveLineOrVisualUp(from, range)
  let l:line = line(a:from)
  if l:line - v:count1 - 1 < 0 | let l:move = '0'
  else                         | let l:move = a:from . ' -' . (v:count1 + 1)
  endif
  call s:moveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
endfunction

function! s:moveLineOrVisualDown(from, range)
  let l:line = line(a:from)
  if l:line + v:count1 > line('$') | let l:move = '$'
  else                             | let l:move = a:from . ' +' . v:count1
  endif
  call s:moveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
endfunction

function! edit#MoveLineUp()
  call s:moveLineOrVisualUp('.', '')
endfunction

function! edit#MoveLineDown()
  call s:moveLineOrVisualDown('.', '')
endfunction

function! edit#MoveVisualUp()
  call s:moveLineOrVisualUp("'<", "'<,'>")
  normal! gv
endfunction

function! edit#MoveVisualDown()
  call s:moveLineOrVisualDown("'>", "'<,'>")
  normal! gv
endfunction

" edit.vim
