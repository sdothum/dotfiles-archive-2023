" sdothum - 2016 (c) wtfpl

" Drawing
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Line styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    let g:linedrawing = '▁▔▂'             " declare multibytes, see statusline.vim

    " ................................................................ Underline

      " insert underline
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      function! Underline(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          normal ^
          let l:col = virtcol('.')
          execute 'normal yypVr' . a:delimiter
          " blank to margin
          if l:col > 1
            execute 'normal ' . l:col . 'r '
          endif
          " prefix lua/haskell comment to avoid potential delimiter conflict
          if &filetype == 'lua' || &filetype == 'haskell'
            normal ^iMark
          endif
          " comment line
          execute "normal :TComment\<CR>"
          if &filetype == 'lua' || &filetype == 'haskell'
            normal /Mark
            " remove prefix+1 to treat as common 2 byte comment leader
            normal 4x$x
          endif
          " place delimiter immediately after comment header (no space)
          " if &filetype != 'lua' && &filetype != 'haskell' && a:delimiter != '='
          "   execute 'normal ^f r' . a:delimiter
          " endif
          " adjust delimiter length by comment leader
          if l:col == 1
            normal $xx
          else
            normal 0x$x
          endif
          normal ^
        endif
      endfunction

      imap <leader><leader>- <C-o>:call Underline('▔')<CR><C-Return>
      nmap <leader><leader>- :call Underline('▔')<CR><Down>
      imap <leader><leader>= <C-o>:call Underline('▂')<CR><C-Return>
      nmap <leader><leader>= :call Underline('▂')<CR><Down>

    " .................................................................... Ruler

      " insert ruler
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      " note: to insert a ruler on an empty line (virtual column position), add a <space>
      " character, else in insert mode, the ruler will positioned using column 1
      function! Drawline(delimiter)
        " insert dummy mark line if on blank line
        if matchstr(getline(line('.')), '\S') == ''
          let l:mark = 1
          normal $RMark
        else
          let l:mark = 0
        endif
        call Underline(a:delimiter)
        " remove temporary mark
        if l:mark > 0
          normal kdd
        endif
        normal $
        if virtcol('.') < g:linewidth
          " for mirrored left/right margin spacing
          " let l:col = g:linewidth - virtcol('.') - l:col + 1
          let l:col = g:linewidth - virtcol('.')
          execute 'normal ' . l:col . 'a' . a:delimiter
        endif
        normal ^
      endfunction

      imap <leader><leader>-- <C-o>:call Drawline('▔')<CR>
      nmap <leader><leader>-- :call Drawline('▔')<CR>
      imap <leader><leader>== <C-o>:call Drawline('▂')<CR>
      nmap <leader><leader>== :call Drawline('▂')<CR>

    " .................................................................. Trailer

      " append trailer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
      function! AppendTrailer(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing trailer
          if matchstr(getline(line('.')), '\s[' . a:delimiter . ']\+$') > ''
            " normal $bhD
            normal $bmD
          endif
          normal $
          let l:col = g:linewidth - virtcol('.') - 1
          if l:col > 0
            " suppress potential comment line wrapping
            set formatoptions-=c
            execute 'normal a '
            execute 'normal ' . l:col . 'a' . a:delimiter
            set formatoptions+=c
          endif
          normal ^
        endif
      endfunction

      imap <leader><leader>_ <C-o>:call AppendTrailer('▁')<CR>
      nmap <leader><leader>_ :call AppendTrailer('▁')<CR>
      imap <leader><leader>. <C-o>:call AppendTrailer('.')<CR>
      nmap <leader><leader>. :call AppendTrailer('.')<CR>

      " prompted trailer
      function! InputTrailer()
        let l:delimiter = input('Line character: ')
        if l:delimiter > ''
          call AppendTrailer(l:delimiter[0])
        endif
      endfunction

      imap <leader><leader>? <C-o>:call InputTrailer()<CR>
      nmap <leader><leader>? :call InputTrailer()<CR>

    " ................................................................... Leader

      " .......................................................... Insert leader
      function! InsertLeader(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing leader
          if matchstr(getline(line('.')), '\S\s\+[' . a:delimiter . ']\+\s') > ''
            " execute 'normal ^wdf '
            execute 'normal ^wdt x'
          endif
          call AppendTrailer(a:delimiter)
          " cut trailer and insert as leader!
          " normal $bhD^whP
          normal $bmD^wmP
          normal ^
        endif
      endfunction

      imap <leader>_ <C-o>:call InsertLeader('▁')<CR>
      nmap <leader>_ :call InsertLeader('▁')<CR>
      imap <leader>. <C-o>:call InsertLeader('.')<CR>
      nmap <leader>. :call InsertLeader('.')<CR>

      " prompted leader
      function! InputLeader()
        let l:delimiter = input('Line character: ')
        if l:delimiter > ''
          if l:delimiter == ' '
            call RightJustify()
          else
            call InsertLeader(l:delimiter[0])
          endif
        endif
      endfunction

      imap <leader>? <C-o>:call InputLeader()<CR>
      nmap <leader>? :call InputLeader()<CR>

      function! RightJustify()
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing leader
          if matchstr(getline(line('.')), '^ *') > ''
            " execute 'normal 0vwhd'
            execute 'normal 0vwmd'
          endif
          normal $
          let l:col = g:linewidth - virtcol('.') - 1
          if l:col > 0
            " suppress potential comment line wrapping
            set formatoptions-=c
            normal ^
            execute 'normal ' . l:col . 'i '
            execute 'normal a '
            set formatoptions+=c
          endif
        endif
      endfunction

      imap <leader><leader>> <C-o>:call RightJustify()<CR>
      nmap <leader><leader>> :call RightJustify()<CR>

" drawing.vim
