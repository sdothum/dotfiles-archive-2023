" sdothum - 2016 (c) wtfpl

" Heading
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Heading styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Underline

      " example: draw underline
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      function! heading#Underline(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          execute 'normal! yypwv$r' . a:delimiter
        endif
        normal! $
      endfunction

    " .................................................................... Ruler

      " example: draw ruler
      " ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
      function! heading#Drawline(delimiter)
        call heading#Underline(a:delimiter)
        if virtcol('.') < g:linewidth       " for mirrored left/right margin spacing
          " let l:col = g:linewidth - virtcol('.') - l:col + 1
          let l:col   = g:linewidth - virtcol('.')
          execute 'normal! ' . l:col . 'a' . a:delimiter
        endif
        normal! $
      endfunction

    " .................................................................. Trailer

      " example: append trailer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

      function! heading#AppendTrailer(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing trailer
          if matchstr(getline(line('.')), '\s[' . a:delimiter . ']\+$') > ''
            normal! $bhD
          endif
          normal! $
          let l:col = g:linewidth - virtcol('.') - 1
          if l:col > 0
            set formatoptions-=c            " suppress potential comment line wrapping
            execute 'normal! a '
            execute 'normal! ' . l:col . 'a' . a:delimiter
            set formatoptions+=c
          endif
          normal! $
        endif
      endfunction

      " prompted trailer
      function! heading#InputTrailer()
        let l:delimiter = input('Line character: ')
        if l:delimiter > ''
          call heading#AppendTrailer(l:delimiter[0])
        endif
      endfunction

    " ................................................................... Leader

      " ................................................. example: insert leader

      function! heading#InsertLeader(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing leader
          if matchstr(getline(line('.')), '\S\s\+[' . a:delimiter . ']\+\s') > ''
            execute 'normal! ^wdf '
          endif
          call heading#AppendTrailer(a:delimiter)
          " cut trailer and insert as leader!
          normal! $bhD^whP
          normal! $
        endif
      endfunction

      " prompted leader
      function! heading#InputLeader()
        let l:delimiter = input('Line character: ')
        if l:delimiter > ''
          if l:delimiter == ' '
            call heading#Justify()
          else
            call heading#InsertLeader(l:delimiter[0])
          endif
        endif
      endfunction

    " .................................................................. Justify

      "                                                         example: justify
      function! heading#Justify()
        execute 's/\v^([ \t]*[^ \t]*)[ \t]*/\1 /'
        call heading#InsertLeader('▔')
        execute ':s/▔/ /'
        normal! $
      endfunction

" heading.vim
