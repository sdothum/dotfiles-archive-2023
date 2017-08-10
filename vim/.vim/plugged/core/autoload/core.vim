" sdothum - 2016 (c) wtfpl

" Primitive
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      " see IndentTheme() plugins.vim
      augroup column
        autocmd!
      augroup END

    " ............................................................. Numeric sort

      " sort compare by numeric (not string) value
      function! core#CompareNumber(i1, i2)
        return a:i1 - a:i2
      endfunction

    " ............................................................... Print file

      " a la vimb
      function! core#Hardcopy()
        if Markdown()
          execute '!hardcopy wiki \"' . expand('%:t') . '\"'
        elseif expand('%:p') =~ 'Patricia'
          execute '!hardcopy wps' expand('%:t')
        else
          execute '!hardcopy code' expand('%:t')
        endif
      endfunction

  " Keyboard layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Colemak-shift-dh

      " Note: scripts are affected by the mappings below!
      "       e.g. "h" becomes "m", "f" becomes "t" etc.
      "       see thedarnedestthing.com

      function! core#Colemak()
        if s:mnle == 0
          " map home row (cluster) cursor movement
          nnoremap k     gk
          vnoremap k     gk
          nnoremap j     gj
          vnoremap j     gj
        else
          " map home row (cluster) cursor movement
          nnoremap u     gk
          vnoremap u     gk
          nnoremap n     h
          vnoremap n     h
          nnoremap e     gj
          vnoremap e     gj
          nnoremap i     l
          vnoremap i     l

          " recover vi keys (including caps for consistency)
          nnoremap f     e
          vnoremap f     e
          nnoremap F     E
          vnoremap F     E
          nnoremap h     m
          vnoremap h     m
          nnoremap k     n
          vnoremap k     n
          nnoremap K     N
          vnoremap K     N

          " combine find and till commands
          nnoremap t     f
          vnoremap t     f
          nnoremap T     F
          vnoremap T     F
          nnoremap <A-t> t
          vnoremap <A-t> t
          nnoremap <C-t> T
          vnoremap <C-t> T
        endif
      endfunction

  " GUI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Column margins

      " hjkl mapping (0) hjkl (1) mnle
      let s:mnle = ("$MNLE" > '' ? $MNLE : 0)

      " toggle colorcolumn modes
      function! core#ToggleColumn()
        if g:ruler == 0
          let g:ruler      = 1
          let &colorcolumn = col('.')
          autocmd column CursorMoved,CursorMovedI * let &colorcolumn = col('.')
        else
          if g:ruler == 1
            let g:ruler = 2
            autocmd! column
          else
            let g:ruler      = 0
            let &colorcolumn = 0
          endif
        endif
        call IndentTheme()
      endfunction

    " ............................................................. Line numbers

      " toggle relative number, line number and no numbering
      function! core#ToggleNumber()
        if (&relativenumber == 1 && &number == 1)
          set norelativenumber
        else
          if (&relativenumber == 0 && &number == 1)
            set nonumber
          else
            set relativenumber
            set number
          endif
        endif
      endfunction

    " ...................................................... White space markers

      " toggle trailing whitespace highlight and indentation levels
      function! core#ToggleSpaces()
        set list!
        if &list == 0
          match ExtraWhitespace /\%x00$/    " nolist by failing match with null character :-)
          " echo ''
          let g:matchspace = ''
        else
          match ExtraWhitespace /\s\+$/
          " echo 'List invisibles ON'
          let g:matchspace = '■'
        endif
      endfunction

  " Buffer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Line wrap

      function! core#ToggleWrap()
        if &formatoptions =~ 't'
          NoPencil
          let &formatoptions = g:codeoptions
          echo 'Automatic line wrap OFF'
        elseif &formatoptions == g:codeoptions
          Pencil
          set formatoptions=tqwan1
          echo 'Automatic line wrap ON'
        else
          set formatoptions
        endif
      endfunction

    " .............................................................. Select text

      function! core#ParagraphAbove()
        if matchstr(getline(line('.')), '\S') == ''
          normal! {
          if matchstr(getline(line('.')), '\S') == ''
            normal! j
          endif
        endif
        normal! }lV{
      endfunction

      function! core#ParagraphBelow()
        if matchstr(getline(line('.')), '\S') == ''
          normal! }
          if matchstr(getline(line('.')), '\S') == ''
            normal! k
          endif
        endif
        normal! {nV}
      endfunction

  " Text ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .......................................................... Shift up / down

      " see https://github.com/gorkunov/vimconfig.git
      function! core#MoveLineUp()
        call core#MoveLineOrVisualUp('.', '')
      endfunction

      function! core#MoveLineDown()
        call core#MoveLineOrVisualDown('.', '')
      endfunction

      function! core#MoveVisualUp()
        call core#MoveLineOrVisualUp("'<", "'<,'>")
        normal! gv
      endfunction

      function! core#MoveVisualDown()
        call core#MoveLineOrVisualDown("'>", "'<,'>")
        normal! gv
      endfunction

      function! core#MoveLineOrVisualUp(line_getter, range)
        let l:line = line(a:line_getter)
        if l:line - v:count1 - 1 < 0
          let l:move = '0'
        else
          let l:move = a:line_getter . ' -' . (v:count1 + 1)
        endif
        call core#MoveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
      endfunction

      function! core#MoveLineOrVisualDown(line_getter, range)
        let l:line = line(a:line_getter)
        if l:line + v:count1 > line('$')
          let l:move = '$'
        else
          let l:move = a:line_getter . ' +' . v:count1
        endif
        call core#MoveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
      endfunction

      function! core#MoveLineOrVisualUpOrDown(move)
        let l:col = virtcol('.')
        execute 'silent! ' . a:move
        execute 'normal! ' . l:col . '|'
      endfunction

    " ......................................................... Insert line wrap

      " insert line while disabling auto-commenting
      function! core#InsertWrap()
        let l:formatoptions = &formatoptions
        set formatoptions-=c
        set formatoptions-=r
        set formatoptions-=o
        normal! ^
        let l:pos = col('.')
        normal! o
        " align line indentation
        execute 'normal! a' . repeat(' ', l:pos)
        let &formatoptions = l:formatoptions
      endfunction

    " .......................................................... Code block text

      " convert wiki text lines into code block lines
      function! core#CodeBlock()
        execute "silent! normal  :s/\\(.*\\)/`\\1`/\<CR>"
        " preserve leading spaces with wiki markdown
        execute "silent! normal! gv:s/^` /`^ /\<CR>"
        execute "silent! normal! gv:s/^``/`^ `/e\<CR>"
        " convert [[ test ]], see thedarnedestthing markdown
        execute "silent! normal! gv:s/ \\[\\[ / [[] /e\<CR>"
        execute "silent! normal! gv:s/ \\]\\] / []] /e\<CR>"
      endfunction

  " Filetype ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Modifiable

      " [ regex name, filetype, modifiable, wordcount ] rule tuple
      " modifiable (0) nomodifiable (1) modifiable
      " wordcount (0) no word count (1) statusline with wordcount
      " note "/name" to represent "^name"
      let s:nametypes =
          \[
          \  [ 'conf$',          'conf',    1, 0 ]
          \, [ 'config$',        'conf',    1, 0 ]
          \, [ 'eml$',           'mail',    1, 1 ]
          \, [ 'error$',         'log',     0, 0 ]
          \, [ 'log$',           'log',     0, 0 ]
          \, [ 'rc$',            'rc',      1, 0 ]
          \, [ 'txt$',           'text',    0, 1 ]
          \, [ 'wiki$',          'vimwiki', 1, 1 ]
          \, [ '\(^\|/\)readme', 'text',    0, 1 ]
          \]

      " [regex fileinfo, filetype, modifiable, readonly] rule tuple
      " modifiable (0) nomodifiable (1) modifiable
      " readonly (0) not set (1) set
      let s:contenttypes =
          \[
          \  [ 'binary',             'binary', 0, 1 ]
          \, [ 'no read permission', 'binary', 0, 1 ]
          \, [ 'text',               'text',   1, 0 ]
          \]

      " set buffer attributes by known filetypes
      function! core#CheckFiletype()
        for [ name, filetype, modifiable, wordcount ] in s:nametypes
          if expand('%') =~ name
            let &filetype   = (&filetype == '' ? filetype : &filetype)
            let &modifiable = modifiable
            break
          endif
        endfor
        if &filetype == ''                  " by file content if not autodetected
          for [ content, filetype, modifiable, readonly ] in s:contenttypes
            if system('file -i ' . expand('%') . '|cut -d: -f2') =~ content
              let &filetype   = filetype
              let &modifiable = modifiable
              let &readonly   = readonly
            endif
          endfor
        endif
        " see Snipmate plugins.vim
        if &filetype == ''
          let &filetype = 'new'
        endif
      endfunction

    " ................................................................... E-mail

      function! core#ComposeMail()
        " email has blank lines inserted externally (via sed) for replys to
        " avoid the previously messy and unpredictable editing mode vim commands
        " see bin/dcompose
        execute 'normal! 4G'
        if matchstr(getline(5), '\S') > ''
          execute 'normal! Yp'
        endif
        execute 'startinsert'
      endfunction

" core.vim
