" sdothum - 2016 (c) wtfpl

" Statusline
" ══════════════════════════════════════════════════════════════════════════════

" At cursor position ___________________________________________________________

" ................................................................... Atom / tag
" attribute at cursor position
function! s:atom()
  return synIDattr(synID(line('.'), col('.'), 1), 'name')
endfunction

command! Atom echo <SID>atom()

function! s:tag()
  return tagbar#currenttag('%s', '')
endfunction

" ............................................................ Special Character
let s:ascii = '\(\d\|\a\|\s\|[`~!@#$%^&*()_\-+={}\[\]\\|;:\",\.<>/?]\)'

function! s:specialChar()
  if mode() == 'n'  " getline() test fails on switch into insert mode
    try
      if !empty(getline(line('.')))            " ignore newline (is NUL)
        let l:char        = getline('.')[col('.')-1]
        if l:char !~ s:ascii && l:char != "'"  " show hex value, not interested in ascii keyboard characters
          let l:statusmsg = v:statusmsg
          normal! ga
          let l:hex       = 'U+' . matchstr(split(v:statusmsg)[3], '[^,]*')
          let v:statusmsg = l:statusmsg
          " clear ga information!
          echo ''
          return l:hex
        endif
      endif
    catch /.*/  " discard messages
    endtry
  endif
  return ''
endfunction

" .................................................................. Cursor info
function! Detail()
  let l:prefix = g:detail ? s:atom() : s:tag()
  return empty(l:prefix) ? s:specialChar() : l:prefix . '  ' . s:specialChar()
endfunction

" Buffer file __________________________________________________________________

" ..................................................................... Pathname
" statusline rootpath, rootname, basename, (filename)
function! s:rootPrefix()
  if expand('%:p') =~ '.*[/][^/]*[/][^/]*[/][^/]*'
    let l:root = substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*[/][^/]*', '\1', '')
    if empty(l:root)
      return l:root
    elseif l:root == substitute(expand('%:p'), '^[/]\([^/]*\)[/].*', '\1', '')
      return l:root
    else
      let l:root = substitute(expand('%:p'), '[/][^/]*[/][^/]*$', '', '')
      return substitute(l:root, $HOME, '~', '')
    endif
  else
    return ''
  endif
endfunction

function! s:rootPath()
  let l:root = substitute(s:rootPrefix(), '[^/]*$', '', '')
  let l:root = substitute(l:root, '\([/][.]*[^/]\)[^/]*', '\1', 'g')
  return substitute(l:root, '[/]', '', 'g')  " abbreviate path prefix and drop slash
endfunction

function! s:rootName()
  return substitute(s:rootPrefix(), '.*[/]\([^/]*\)$', '\1', '')
endfunction

" current directory
function! s:baseName()
  if expand('%:p') =~ '.*[/][^/]*[/][^/]*' | return substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*', '\1', '')
  else                                     | return '' | endif
endfunction

" Buffer statistics ____________________________________________________________

" ................................................................... Word count
" null return for non-prose or empty new buffer
function! s:wordCount()
  if mode() =~ '[vV]' | return '' | endif  " visual mode (gives incorrect word count)
  try  " trap error caused by snippet expansion involving substition placeholders
    let b:wordcount = ''
    let l:statusmsg = v:statusmsg
    let l:position  = getpos('.')
    execute "silent normal! g\<C-g>"
    if v:statusmsg != '--No lines in buffer--' | let b:wordcount = str2nr(split(v:statusmsg)[11]) | endif
    let v:statusmsg = l:statusmsg
    call setpos('.', l:position)  " go back (to EOL if need be)
    return b:wordcount
  catch /.*/  " discard messages
  endtry
endfunction

" Statusline content ___________________________________________________________

" ....................................................................... Leader
function! s:escape(text)
  return substitute('%*' . a:text, ' ', '\\ ', 'g')
endfunction

function! Leader(text)
  return repeat(' ', (winwidth(0) / 2) - strlen(a:text) - strlen(g:pad[0]))
endfunction

" .................................................................... Left side
function! Name()
  return expand('%:t' . (Prose() ? ':r' : ''))
endfunction

function! Path()
  let l:path = s:rootPath() . '/' . s:rootName() . '/' . s:baseName()
  let l:path = substitute(l:path, $HOME, '~/', '')
  let l:path = substitute(l:path, '//', '/', '')
  return l:path
endfunction

" ................................................................. Buffer state
function! s:attn()
  return system('stat --printf %U ' . expand('%:p')) == 'root' ? '%3*' : '%1*'
endfunction

function! UnModified(show)
  " return &modifiable ? (&modified ? g:icon[2] : a:show ? g:icon[0] : '') : g:icon[1]
  return (expand('%t') =~ 'NrrwRgn' || w:tagged == g:active) ? (&modifiable ? (&modified ? g:icon[2] : a:show ? g:icon[0] : '') : g:icon[1]) : g:icon[3]
endfunction

" ................................................................... Right side
" normal mode code: col -> file%, prose: col -> wordcount
" insert mode code: col 
function! PosWordsCol()
  return mode() == 'n' ? (g:show_column ? col('.') : (Prose() ? s:wordCount() : (line('.') * 100 / line('$')) . '%')) : col('.')
endfunction

" DFM / expanded _______________________________________________________________

" .............................................................. set statusline=
" [path] .. filename state position .. [details]
function! Statusline(expanded)
  try  " trap snippet insertion interruption
    if Prose() && g:duochrome_insert
      return s:escape(s:attn() . Leader('') . '  %{UnModified(0)}%1*')
    else
      let l:name     = '%{Name()}' . g:pad[0]
      if a:expanded  " center dfm indicator / proofing statusline
        let l:path   = '%{Path()}'
        let l:leader = '%{Leader(Path() . g:pad[1] . Name())}'
      else
        let l:leader = '%{Leader(Name())}'
      endif
      let l:name     = '%1*' . l:name
      let l:info     = s:attn() . '%{UnModified(1)}' . g:pad[0] . '%1*%{PosWordsCol()}'  " utf-8 symbol occupies 2 chars (pad right 1 space)
      if a:expanded
        let l:name   = '%2*' . l:path . '%1*' . g:pad[1] . l:name
        let l:info  .= g:pad[1] . '%2*%{Detail()}'
      endif
      return s:escape('%1*' . l:leader . l:name . l:info . '%1*')
    endif
  catch /.*/  " discard messages
  endtry
endfunction

" statusline.vim
