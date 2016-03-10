" sdothum - 2016 (c) wtfpl

" Drawing
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Line styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    let g:linedrawing = '▁▔▂'             " declare multibytes, see statusline.vim

    " ................................................................ Underline

      " example: draw underline
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔

      function! Underline(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          normal ^
          let l:col = virtcol('.')
          execute 'normal yypVr' . a:delimiter
          if l:col > 1                      " blank to margin
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

      " example: draw ruler
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      " note: to insert a ruler on an empty line (virtual column position), add a <space>
      " character, else in insert mode, the ruler will positioned using column 1

      function! Drawline(delimiter)
        if matchstr(getline(line('.')), '\S') == '' " insert dummy mark line if on blank line
          let l:mark = 1
          normal $RMark
        else
          let l:mark = 0
        endif
        call Underline(a:delimiter)
        if l:mark > 0                       " remove temporary mark
          normal kdd
        endif
        normal $
        if virtcol('.') < g:linewidth       " for mirrored left/right margin spacing
          " let l:col = g:linewidth - virtcol('.') - l:col + 1
          let l:col   = g:linewidth - virtcol('.')
          execute 'normal ' . l:col . 'a' . a:delimiter
        endif
        normal ^
      endfunction

      imap <leader><leader>-- <C-o>:call Drawline('▔')<CR>
      nmap <leader><leader>-- :call Drawline('▔')<CR>
      imap <leader><leader>== <C-o>:call Drawline('▂')<CR>
      nmap <leader><leader>== :call Drawline('▂')<CR>

    " .................................................................. Trailer

      " example: append trailer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

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
            set formatoptions-=c            " suppress potential comment line wrapping
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

      " ................................................. example: insert leader

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
          if matchstr(getline(line('.')), '^ *') > '' " remove existing leader
            " execute 'normal 0vwhd'
            execute 'normal 0vwmd'
          endif
          normal $
          let l:col = g:linewidth - virtcol('.') - 1
          if l:col > 0
            set formatoptions-=c            " suppress potential comment line wrapping
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
