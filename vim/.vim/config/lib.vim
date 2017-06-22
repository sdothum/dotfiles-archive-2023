" sdothum - 2016 (c) wtfpl

" Functions
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ....................................................... Error message trap

      " ignore 1st time error messages from plugins (uninitialized s:variables)
      function! Quietly(command)
        try
          execute a:command
        catch /.*/
          " discard messages, do nothing
        endtry
      endfunction

    " ............................................................. Numeric sort

      " sort compare by numeric (not string) value
      function! s:CompareNumber(i1, i2)
        return a:i1 - a:i2
      endfunction

      function! SortNumbers(nums)
        return sort(a:nums, '<SID>CompareNumber')
      endfunction

    " ......................................................... Strip whitespace

      " see https://dougblack.io/words/a-good-vimrc.html
      " strips trailing whitespace from all lines
      function! StripTrailingWhitespaces()
        if &modifiable == 1 && ! Markdown()
          " save last search & cursor position
          let _s = @/
          let l  = line(".")
          let c  = col(".")
          %s/\s\+$//e
          let @/ = _s
          call cursor(l, c)
        endif
      endfunction

  " Files ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Prose filestypes

      " distraction free filetyes
      function! Prose()
        return &filetype =~ 'vimwiki\|mail\|markdown\|draft'
      endfunction

      function! Markdown()
        return &filetype =~ 'vimwiki\|markdown'
      endfunction

      " ..................................................... Sync arm nfs share

      " arm device mirrors ~/stow directory with nfs share:/stow for ~/stow performance
      function! SyncNFS()
        if &modified
          " vim backups added by syncnfs
          execute ':silent !syncnfs ' . expand('%:p')
        endif
      endfunction

    " Status line ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................... Modified notification

      " replaces lightline: 'modified' : '%{&filetype == "help" ? "" : &modified ? "+" : &modifiable ? "" : "⎯"}'
      function! Modified(...)
        let unmod = a:0 > 0 ? ' ' : ''
        if &filetype == 'help'
          return unmod
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
          return unmod
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
          let root = substitute(expand('%:p'), '.*[/]\([^/]*\)[/][^/]*[/][^/]*', '\1', '')
          if root == ''
            return root
          else
            if root == substitute(expand('%:p'), '^[/]\([^/]*\)[/].*', '\1', '')
              return root
            else
              let root = substitute(expand('%:p'), '[/][^/]*[/][^/]*$', '', '')
              let root = substitute(root, $HOME, '~', '')
              let base = substitute(root, '.*[/]\([^/]*\)$', '\1', '')
              let root = substitute(root, '[^/]*$', '', '')
              let root = substitute(root, '\([/][.]*[^/]\)[^/]*', '\1', 'g')
              return root . base
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

" functions.vim
