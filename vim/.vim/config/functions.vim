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

    " .......................................................... Escape filename
    
      " change space to \<Space>, see Print (buffers.vim)
      function! EscapeFilename()
        return substitute(expand('%:t'), " ", "\\\\ ", "g")
      endfunction

    " ......................................................... Strip whitespace
     
      " see https://dougblack.io/words/a-good-vimrc.html 
      " strips trailing whitespace from all lines
      function! StripTrailingWhitespaces()
        " save last search & cursor position
        let _s=@/
        let l = line(".")
        let c = col(".")
        %s/\s\+$//e
        let @/=_s
        call cursor(l, c)
      endfunction

" functions.vim
