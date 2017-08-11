" sdothum - 2016 (c) wtfpl

" Info
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Statusline ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      " including multibyte characters used for line drawing, see heading.vim
      " note: files using other multibyte characters will produce incorrect statistics
      let s:indicators = '▶■≈⌉'             " multibyte statusline indicators
      let s:multibytes = '[' . g:multibytes . s:indicators . ']'

  " Buffer statistics ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Line info

      " see https://github.com/scrooloose/vimfiles/blob/master/vimrc#L78
      " return a warning for long lines > g:linewidth
      function! info#LineSizes()
        " return 'x +/y z' if long lines are found, where
        "   x is the number of long lines
        "   y is the median length of the long lines
        "   z is the length of the longest line
        if !exists('b:statusline_long_line_warning')
          let b:statusline_long_line_warning = ''
          if &modifiable
            let long_line_lens = info#LongLines()
            if len(long_line_lens) > 0
              let b:statusline_long_line_warning =
                  \         len(long_line_lens) . '>'
                  \. ' ≈' . info#Median(long_line_lens)
                  \. ' ⌉'  . max(long_line_lens)
            else
              let b:statusline_long_line_warning = '0>'
            endif
          endif
        endif
        return b:statusline_long_line_warning
      endfunction

      " return a list containing the lengths of the long lines in this buffer
      function! info#LongLines()
        let l:spaces    = repeat(' ', &tabstop)
        " trap multibyte line drawing characters used by "ruler" and "underline"
        let l:line_lens = map(getline(1,'$'),
            \ 'len(substitute(v:val =~ s:multibytes
            \? substitute(v:val, s:multibytes, " ", "g")
            \: v:val, "\\t", l:spaces, "g"))')
        return filter(l:line_lens, 'v:val > g:linewidth')
      endfunction

      " find the median of the given array of numbers
      function! info#Median(nums)
        let l:nums     = SortNumbers(a:nums) " original code incorrectly sorted by text
        let l:size     = len(l:nums)
        if l:size % 2 == 1
          let l:middle = (l:size-1)/2
          return l:nums[l:middle]
        else
          return (l:nums[l:size/2] + l:nums[(l:size/2)-1]) / 2
        endif
      endfunction

" info.vim
