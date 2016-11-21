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

    " .......................................................... Modified status

      function! Modified()
        if &filetype == "help"
          return ''
        endif
        if &modified
          if b:modified == 0
            call system('time=10 notify critical "' . expand('%:r') . '" "Modified"')
            let b:modified = 1
          endif
          return '+'
        endif
        let b:modified = 0
        if &modifiable
          return ''
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

    " .......................................................... Escape filename

      " change space to \<Space>, see Print (buffers.vim)
      function! EscapeFilename()
        return substitute(expand('%:t'), " ", "\\\\ ", "g")
      endfunction

    " ......................................................... Strip whitespace

      " see https://dougblack.io/words/a-good-vimrc.html
      " strips trailing whitespace from all lines
      function! StripTrailingWhitespaces()
        if &modifiable == 1
          " save last search & cursor position
          let _s=@/
          let l = line(".")
          let c = col(".")
          %s/\s\+$//e
          let @/=_s
          call cursor(l, c)
        endif
      endfunction

    " ...................................................................... GUI

      " toggle gui menu
      function! ToggleGui()
        if &guioptions =~# 'T'
          set guioptions-=T
          set guioptions-=m
        else
          set guioptions+=T
          set guioptions+=m
        endif
      endfunction


    " .................................................................. Sneak f

      " remap sneak_s to preserve s
      function! Sneak_f()
        if !exists("g:sneak_f")
          let g:sneak_f = 1
          unmap s
          unmap S
          call Colemak()
          nmap f <Plug>Sneak_s
          nmap F <Plug>Sneak_S
        endif
      endfunction

" functions.vim
