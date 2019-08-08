" sdothum - 2016 (c) wtfpl

" Info
" ══════════════════════════════════════════════════════════════════════════════

  " Buffer info ________________________________________________________________

    " ..................................................................... Atom

      " attribute at cursor position
      function! info#Atom()
        return synIDattr(synID(line('.'), col('.'), 1), 'name')
      endfunction

    " ...................................................................... Tag

      function! info#Tag()
        return tagbar#currenttag('%s', '')
      endfunction

    " ........................................................ Special Character

      let s:ascii = '\(\d\|\a\|\s\|[`~!@#$%^&*()_\-+={}\[\]\\|;:\",\.<>/?]\)'

      function! info#SpecialChar()
        if mode() == 'n'  " getline() test fails on switch into insert mode
          try
            if getline(line('.')) != ''  " ignore newline (is NUL)
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

    " ................................................................. Pathname

      " statusline rootpath, rootname, basename, (filename)
      function! s:rootPrefix()
        if expand('%:p') =~ '.*[/][^/]*[/][^/]*[/][^/]*'
          let l:root = substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*[/][^/]*', '\1', '')
          if l:root == ''
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

  " Buffer statistics __________________________________________________________

    " ............................................................. Buffer count

      function! info#BufCount()
        return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
      endfunction

    " ............................................................... Word count

      " null return for non-prose or empty new buffer, see http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
      function! s:wordCount()
        if expand('%:t') == 'ControlP' || mode() =~ '[vV]' | return '' | endif  " trap buffer window, visual mode (gives incorrect word count)
        try  " trap error caused by snippet expansion involving substition placeholders
          let b:wordcount = ''
          let l:statusmsg = v:statusmsg
          let l:position  = getpos('.')
          execute "silent normal! g\<C-g>"
          if v:statusmsg != '--No lines in buffer--' | let b:wordcount = str2nr(split(v:statusmsg)[11]) | endif
          let v:statusmsg   = l:statusmsg
          call setpos('.', l:position)  " go back (to EOL if need be)
          return b:wordcount
        catch /.*/  " discard messages
        endtry
      endfunction

  " DFM statusline _____________________________________________________________

    " ............................................................... File state

      function! info#UnModified(show)
        " return &modifiable ? (&modified ? g:modified_ind : a:show == 1 ? g:unmodified_ind : '') : g:unmodifiable_ind
        return (w:tagged == g:active) ? (&modifiable ? (&modified ? g:modified_ind : a:show == 1 ? g:unmodified_ind : '') : g:unmodifiable_ind) : ' '
      endfunction

    " ................................................................ Left path

      function! info#Name()
        return expand('%:t' . (core#Prose() ? ':r' : ''))
      endfunction

      function! info#Path()
        let l:path = s:rootPath() . '/' . s:rootName() . '/' . s:baseName()
        let l:path = substitute(l:path, $HOME, '~/', '')
        let l:path = substitute(l:path, '//', '/', '')
        return l:path
      endfunction

    " ............................................................... Right edit

      " normal mode code: col -> file%, prose: col -> wordcount
      " insert mode code: col 
      function! info#PosWordsCol()
        return mode() == 'n' ? (g:column == 0 ? (core#Prose() ? s:wordCount() : (line('.') * 100 / line('$')) . '%') : col('.')) : col('.')
      endfunction

    " ........................................................... Escaped leader

      function! info#Leader(text)
        return repeat(' ', (winwidth(0) / 2) - strlen(a:text) - strlen(g:pad_inner))
      endfunction

      function! info#Escape(text)
        return substitute('%*' . a:text, ' ', '\\ ', 'g')
      endfunction

" info.vim
