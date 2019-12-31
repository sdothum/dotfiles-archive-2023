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

    " ......................................................... Prose filestypes

      " distraction free filetyes
      function! Prose()
        return &filetype =~ 'vimwiki\|mail\|markdown\|draft'
      endfunction

      function! Markdown()
        return &filetype =~ 'vimwiki\|markdown'
      endfunction

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

" functions.vim
