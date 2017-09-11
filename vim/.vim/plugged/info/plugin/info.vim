" sdothum - 2016 (c) wtfpl

" Info
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Statusline ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      if exists("g:loaded_info")
        finish
      endif
      let g:loaded_info = 1
      let s:save_cpo = &cpo
      set cpo&vim

      " external toggles (to be further refined)
      let g:code  = 0                       " statusline buffer statistics toggle (0) off (1) on
      let g:prose = 0

      augroup info
        autocmd!
      augroup END

  " Buffer info ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................... Modified notification

      " replaces lightline: 'modified' : '%{&filetype == "help" ? "" : &modified ? "+" : &modifiable ? "" : "⎯"}'
      function! Modified(...)
        let l:unmod = a:0 > 0 ? ' ' : ''
        if &filetype == 'help'
          return l:unmod
        endif
        if &modified
          if b:modified == 0
            " path must have one parent directory i.e. does not resolve /root filenames
            let l:rootpath = expand('%:p') =~ '.*[/][^/]*[/][^/]*[/][^/]*'
                \? substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*[/][^/]*', '\1', '') . '/'
                \: '/'
            let l:basepath = expand('%:p') =~ '.*[/][^/]*[/][^/]*'
                \? substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*', '\1', '') . '/'
                \: ''
            call system('notify time=3 critical "' . l:rootpath . l:basepath . expand('%:t') . '" "Modified"')
            let b:modified = 1
          endif
          return '+'
        endif
        let b:modified = 0
        if &modifiable
          return l:unmod
        endif
        return '-'
      endfunction

    " ..................................................................... Atom

      " attribute at cursor position
      function! Atom()
        return synIDattr(synID(line('.'), col('.'), 1), 'name')
      endfunction

      function! TopBottom()
        if line('w0') == 1
          return line('w$') == line('$') ? '' : '▼'
        else
          return line('w$') == line('$') ? '▲' : ''
        endif
      endfunction

    " ................................................................. Pathname

      " abbreviated path spec
      function! RootPath()
        if expand('%:p') =~ '.*[/][^/]*[/][^/]*[/][^/]*'
          " return substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*[/][^/]*', '\1', '')
          let l:root = substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*[/][^/]*', '\1', '')
          if l:root == ''
            return l:root
          else
            if l:root == substitute(expand('%:p'), '^[/]\([^/]*\)[/].*', '\1', '')
              return l:root
            else
              let l:root = substitute(expand('%:p'), '[/][^/]*[/][^/]*$', '', '')
              let l:root = substitute(l:root, $HOME, '~', '')
              let l:base = substitute(l:root, '.*[/]\([^/]*\)$', '\1', '')
              let l:root = substitute(l:root, '[^/]*$', '', '')
              let l:root = substitute(l:root, '\([/][.]*[^/]\)[^/]*', '\1', 'g')
              return l:root . l:base
            endif
          endif
        else
          return ''
        endif
      endfunction

      " current directory
      function! BasePath()
        if expand('%:p') =~ '.*[/][^/]*[/][^/]*'
          return substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*', '\1', '')
        else
          return ''
        endif
      endfunction

  " Buffer statistics ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................. Byte position percentage

      " let s:super = ['^', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹']

      function! LinePercent()
        let l:line = line('.') - 1
        let l:last = line('$') - 1
        " return (l:line != 0 && l:line != l:last) ? '╻' . s:super[l:line * 10 / l:last] : ''
        return (l:line != 0 && l:line != l:last) ? '╻' . (l:line * 10 / l:last) : ''
      endfunction

    " ................................................................ Line info

      function! LineSizes()
        if g:code == 1
          return info#LineSizes()
        endif
        return ''
      endfunction

      " line statistics off by default per buffer (declare flags to b:uffer)
      " autocmd info BufRead               * let b:code = 0
      " recalculate the long line warning when idle and after saving
      autocmd info CursorHold,BufWritePost * unlet! b:statusline_long_line_warning

    " ............................................................... Word count

      " see http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
      " null return suppresses wordcount for non-prose or empty new buffer
      function! WordCount()
        " trap buffer window, visual mode (gives incorrect word count)
        if expand('%:t') == 'ControlP' || mode() =~ '[vV]'
          return ''
        endif
        if g:code == 0 && g:prose == 0
          return ''
        endif
        let b:wordcount   = ''
        let l:statusmsg   = v:statusmsg
        let l:position    = getpos('.')     " g<C-g> prevents (cursor from) appending to EOL in vim 7.4
        execute "silent normal! g\<C-g>"
        if v:statusmsg != '--No lines in buffer--'
          let b:wordcount = str2nr(split(v:statusmsg)[11])
        endif
        let v:statusmsg   = l:statusmsg
        " go back (to EOL if need be)
        call setpos('.', l:position)
        return b:wordcount
      endfunction

    " ........................................................ Special Character

      let s:ascii = '\(\d\|\a\|\s\|[`~!@#$%^&*()_\-+={}\[\]\\|;:\",\.<>/?]\)'

      function! SpecialChar()
        if mode() == 'n'                    " getline() test fails on switch into insert mode
          try
            if getline(line('.')) != ''     " ignore newline (is NUL)
              let l:char        = getline('.')[col('.')-1]
              " show hex value, not interested in ascii keyboard characters
              if l:char !~ s:ascii && l:char != "'"
                let l:statusmsg = v:statusmsg
                normal! ga
                let l:hex       = 'U+' . matchstr(split(v:statusmsg)[3], '[^,]*')
                let v:statusmsg = l:statusmsg
                " clear ga information!
                echo ''
                return l:hex
              endif
            endif
          catch /.*/
            " discard messages and clear message line
            echo ''
          endtry
        endif
        return ''
      endfunction

    " ................................................................. Warnings

      " see https://github.com/scrooloose/vimfiles/blob/master/vimrc#L78
      " inconsistent tab warning, return '&expandtab' if &expandtab is set wrong
      " return '|▶' if spaces and tabs are used to indent
      function! Indent()
        if !exists('b:statusline_tab_warning')
          let b:statusline_tab_warning = ''
          if &modifiable
            let l:tabs = search('^\t', 'nw') != 0
            "find spaces that arent used as alignment in the first indent column
            let l:spaces = search('^ \{' . &tabstop . ',}[^\t]', 'nw') != 0
            if l:tabs && l:spaces
              let b:statusline_tab_warning = '|▶'
            elseif (l:spaces && !&expandtab) || (l:tabs && &expandtab)
              let b:statusline_tab_warning = '&expandtab'
            endif
          endif
        endif
        return b:statusline_tab_warning
      endfunction

      " trailing spaces warning, return '■|' if trailing spaces/tabs are present
      function! Spaces()
        if !exists('b:statusline_pad_warning')
          let b:statusline_pad_warning = ''
          if &modifiable
            if g:prose == 0
              if search('[ \t]\+$', 'nw') != 0
                let b:statusline_pad_warning = '■|'
              endif
            endif
          endif
        endif
        return b:statusline_pad_warning
      endfunction

      " recalculate the tab warning flag when idle and after writing
      autocmd info CursorHold,BufWritePost * unlet! b:statusline_tab_warning
      " recalculate the trailing whitespace warning when idle, and after saving
      autocmd info CursorHold,BufWritePost * unlet! b:statusline_pad_warning

      let &cpo = s:save_cpo
      unlet s:save_cpo

" info.vim
