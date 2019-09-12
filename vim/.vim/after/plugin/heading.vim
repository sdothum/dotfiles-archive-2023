" sdothum - 2016 (c) wtfpl

" Heading
" ══════════════════════════════════════════════════════════════════════════════

" Heading styles _______________________________________________________________

" .................................................................... Underline
" example: draw underline
" ───────────────────────
function! s:underline(delimiter)
  if NonBlankLine() | execute 'normal! yypwv$r' . a:delimiter | endif
  normal! $
endfunction

command! -nargs=1 Underline silent! call <SID>underline(<f-args>)

" ........................................................................ Ruler
" example: draw ruler
" ──────────────────────────────────────────────────────────────────────────────
function! s:drawline(delimiter)
  call s:underline(a:delimiter)
  if virtcol('.') < g:linewidth  " for mirrored left/right margin spacing
    let l:col   = g:linewidth - virtcol('.')
    execute 'normal! ' . l:col . 'a' . a:delimiter
  endif
  normal! $
endfunction

command! -nargs=1 Drawline silent! call <SID>drawline(<f-args>)

" Sub-heading styles ___________________________________________________________

" ...................................................................... Trailer
" example: append trailer ______________________________________________________
function! s:appendTrailer(delimiter)
  if NonBlankLine()
    if matchstr(getline(line('.')), '\s[' . a:delimiter . ']\+$') > ''  " remove existing trailer
      normal! $bhD
    endif
    normal! $
    let l:col = g:linewidth - virtcol('.') - 1
    if l:col > 0
      set formatoptions-=c  " suppress potential comment line wrapping
      execute 'normal! a '
      execute 'normal! ' . l:col . 'a' . a:delimiter
      set formatoptions+=c
    endif
    normal! $
  endif
endfunction

command! -nargs=1 AppendTrailer silent! call <SID>appendTrailer(<f-args>)

" prompted trailer
function! s:InputTrailer()
  let l:delimiter = input('Line character: ')
  if l:delimiter > '' | call s:appendTrailer(l:delimiter[0]) | endif
endfunction

command! InputTrailer silent! call <SID>inputTrailer()

" ....................................................................... Leader
" ....................................................... example: insert leader
function! s:insertLeader(delimiter)
  if NonBlankLine()
    if matchstr(getline(line('.')), '\S\s\+[' . a:delimiter . ']\+\s') > '' | execute 'normal! ^wdf ' | endif  " remove existing leader
    call s:appendTrailer(a:delimiter)
    " cut trailer and insert as leader!
    normal! $bhD^whP
    normal! $
  endif
endfunction

command! -nargs=1 InsertLeader silent! call <SID>insertLeader(<f-args>)

" prompted leader
function! s:inputLeader()
  let l:delimiter = input('Line character: ')
  if l:delimiter > ''
    if l:delimiter == ' ' | call s:justify()
    else                  | call s:insertLeader(l:delimiter[0]) | endif
  endif
endfunction

command! InputLeader silent! call <SID>inputLeader()

" ...................................................................... Justify
"                                                               example: justify
function! s:justify()
  execute 's/\v^([ \t]*[^ \t]*)[ \t]*/\1 /'
  call s:insertLeader('▔')
  execute ':s/▔/ /'
  normal! $
endfunction

command! Justify silent! call <SID>justify()

" heading.vim
