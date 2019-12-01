" sdothum - 2016 (c) wtfpl

" Edit
" ══════════════════════════════════════════════════════════════════════════════

" Buffer _______________________________________________________________________

" ..................................................................... Filetype
" set corresponding ui for filetye
function! s:filetype(ft)
  execute 'setfiletype ' . a:ft
  Layout  " respect ui
endfunction

command! -nargs=1 Filetype silent! call <SID>filetype(<f-args>)

" Line _________________________________________________________________________

" .................................................................. Insert line
" insert line while disabling auto-commenting OR break (prose) line
function! s:smartWrap()
  if Prose()  " override Pencil mode (the default state for prose)
    set paste
    execute "normal! i\<CR>"
    set nopaste
    execute 'startinsert'
  else  " append EOL wrap from any col position
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

command! SmartWrap silent! call <SID>smartWrap()

" ............................................................. Strip whitespace
" strips trailing whitespace from all lines
function! s:stripTrailingWhitespaces()
  if &modifiable && !Markdown()
    " let l:_s = @/ " save last search & cursor position
    " let l:l  = line(".")
    " let l:c  = col(".")
    let s:view = winsaveview()
    %s/\s\+$//e           " EOL
    %s/\(\n\r\?\)\+\%$//e " EOF
    call winrestview(s:view)
    " let @/ = l:_s
    " call cursor(l:l, l:c)
  endif
endfunction

command! StripTrailingWhitespaces silent! call <SID>stripTrailingWhitespaces()

" Convert text _________________________________________________________________

" ................................................................. Convert tabs
command! -range=% Tab2Space silent! execute '<line1>,<line2>s#^\t\+#\=repeat(" ", len(submatch(0))*' . &ts . ')'
command! -range=% Space2Tab silent! execute '<line1>,<line2>s#^\( \{'.&ts.'\}\)\+#\=repeat("\t", len(submatch(0))/' . &ts . ')'

" .............................................................. Code block text
" convert wiki text lines into code block lines
function! s:codeBlock()
  execute "silent! normal  :s/\\(.*\\)/`\\1`/\<CR>"
  " preserve leading spaces with wiki markdown
  execute "silent! normal! gv:s/^` /`^ /\<CR>"
  execute "silent! normal! gv:s/^``/`^ `/e\<CR>"
  " convert [[test]], see thedarnedestthing markdown
  execute "silent! normal! gv:s/ \\[\\[ / [[] /e\<CR>"
  execute "silent! normal! gv:s/ \\]\\] / []] /e\<CR>"
endfunction

command! -range=% -nargs=0 CodeBlock silent! execute '<line1>,<line2>call <SID>codeBlock()'

" Text shift ___________________________________________________________________

" .................................................................. Select text
function! s:paragraphAbove()
  if NonBlankLine
    normal! {
    if BlankLine()
      normal! j
    endif
  endif
  normal! }kV{
endfunction
  
command! ParagraphAbove silent! call <SID>paragraphAbove()

function! s:paragraphBelow()
  if NonBlankLine
    normal! }
    if BlankLine()
      normal! k
    endif
  endif
  normal! {jV}
endfunction
  
command! ParagraphBelow silent! call <SID>paragraphBelow()

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
  else                         | let l:move = a:from . ' -' . (v:count1 + 1) | endif
  call s:moveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
endfunction

function! s:moveLineOrVisualDown(from, range)
  let l:line = line(a:from)
  if l:line + v:count1 > line('$') | let l:move = '$'
  else                             | let l:move = a:from . ' +' . v:count1 | endif
  call s:moveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
endfunction

function! s:moveLineUp()
  call s:moveLineOrVisualUp('.', '')
endfunction
  
command! MoveLineUp silent! call <SID>moveLineUp()

function! s:moveLineDown()
  call s:moveLineOrVisualDown('.', '')
endfunction
  
command! MoveLineDown silent! call <SID>moveLineDown()

function! s:moveVisualUp()
  call s:moveLineOrVisualUp("'<", "'<,'>")
  normal! gv
endfunction
  
command! MoveVisualUp silent! call <SID>moveVisualUp()

function! s:moveVisualDown()
  call s:moveLineOrVisualDown("'>", "'<,'>")
  normal! gv
endfunction
  
command! MoveVisualDown silent! call <SID>moveVisualDown()

" edit.vim
