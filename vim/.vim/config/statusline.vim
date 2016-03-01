" sdothum - 2016 (c) wtfpl

" Statusline
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Content extensions ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Line info

      " regex list of multibyte characters used for line drawing
      " note: files using other multibyte characters will produce incorrect statistics
      let s:multibytes = '[─═‾↑▁▔▂]'

      " see https://github.com/scrooloose/vimfiles/blob/master/vimrc#L78
      " return a warning for long lines > g:linewidth
      function! s:LineInfo()
        " return 'x +/y z' if long lines are found, where
        "   x is the number of long lines
        "   y is the median length of the long lines
        "   z is the length of the longest line
        if !exists('b:statusline_long_line_warning')
          let b:statusline_long_line_warning = ''
          if &modifiable
            let long_line_lens = s:LongLines()
            if len(long_line_lens) > 0
              let b:statusline_long_line_warning =
                \          len(long_line_lens) . '='
                \ . ' ‾' . s:Median(long_line_lens)
                \ . ' '  . max(long_line_lens) . '↑'
            else
              let b:statusline_long_line_warning = ''
            endif
          endif
        endif
        return b:statusline_long_line_warning
      endfunction

      " return a list containing the lengths of the long lines in this buffer
      function! s:LongLines()
        let l:spaces = repeat(' ', &tabstop)
        " let l:line_lens = map(getline(1,'$'), 'len(substitute(v:val, "\\t", l:spaces, "g"))')
        " trap multibyte line drawing characters used by "ruler" and "underline"
        " let l:line_lens = map(getline(1,'$'), 'v:val =~ s:multibytes
        "   \ ? len(substitute(substitute(v:val, s:multibytes, " ", "g"), "\\t", l:spaces, "g"))
        "   \ : len(substitute(v:val, "\\t", l:spaces, "g"))')
        let l:line_lens = map(getline(1,'$'), 'len(substitute(v:val =~ s:multibytes ? substitute(v:val, s:multibytes, " ", "g") : v:val, "\\t", l:spaces, "g"))')
        return filter(l:line_lens, 'v:val > g:linewidth')
      endfunction

      " find the median of the given array of numbers
      function! s:Median(nums)
        " original code incorrectly sorted by text
        let l:nums = SortNumbers(a:nums)
        " echo l:nums
        let l:size = len(l:nums)
        if l:size % 2 == 1
          let l:middle = (l:size-1)/2
          return l:nums[l:middle]
        else
          return (l:nums[l:size/2] + l:nums[(l:size/2)-1]) / 2
        endif
      endfunction

      function! LineInfo()
        " plugin command windows bypass autocmds
        if exists('b:code')
          if b:code == 1
            return s:LineInfo()
          endif
        endif
        return ''
      endfunction

      " line statistics off by default per buffer
      autocmd bufread                 * let b:code = 0
      " recalculate the long line warning when idle and after saving
      autocmd cursorhold,bufwritepost * unlet! b:statusline_long_line_warning

    " ............................................................... Word count

      " persistent vimwiki wordcount statusline
      let g:wikistatus = 0                  " default vimwiki statusline off

      " see http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
      " null return suppresses wordcount for non-prose or empty new buffer
      function! s:WordCount()
        let b:wordcount = ''
        let l:statusmsg = v:statusmsg
        " g<C-g> prevents (cursor from) appending to EOL in vim 7.4
        let l:position = getpos('.')
        execute "silent normal g\<C-g>"
        if v:statusmsg != '--No lines in buffer--'
          let b:wordcount = str2nr(split(v:statusmsg)[11])
        endif
        let v:statusmsg = l:statusmsg
        " go back (to EOL if need be)
        call setpos('.', l:position)
        return b:wordcount
      endfunction

      function! WordCount()
        " plugin command windows bypass autocmds
        if exists('b:prose')
          " ignore source code word counts
          if b:prose == 1
            return s:WordCount()
          endif
        endif
        return ''
      endfunction

      " toggle word count manually
      imap <silent><A-F10> <C-o>:let b:prose = (b:prose == 0 ? 1 : 0)<CR>
      nmap <silent><A-F10> :let b:prose = (b:prose == 0 ? 1 : 0)<CR>

    " ........................................................ Special Character

      function! SpecialChar()
        " getline() test fails on switch into insert mode
        if mode() == 'n'
          " ignore newline (is NUL)
          if getline(line('.')) != ''
            let l:char = getline('.')[col('.')-1]
            " not interested in ascii keyboard characters
            if l:char !~ '\(\d\|\a\|\s\|[`~!@#$%^&*()_\-+={}\[\]\\|;:\",\.<>/?]\)' && l:char != "'"
              let l:statusmsg = v:statusmsg
              normal ga
              " show hex value :-)
              let l:hex = 'U+' . matchstr(split(v:statusmsg)[3], '[^,]*')
              let v:statusmsg = l:statusmsg
              " clear ga information!
              echo ''
              return l:hex
            endif
          endif
        endif
        return ''
      endfunction

    " ................................................................. Warnings

      " see https://github.com/scrooloose/vimfiles/blob/master/vimrc#L78
      " inconsistent tab warning
      function! Indent()
        " return '&expandtab' if &expandtab is set wrong
        " return '^ indent' if spaces and tabs are used to indent
        if !exists('b:statusline_tab_warning')
          let b:statusline_tab_warning = ''
          if &modifiable
            let l:tabs = search('^\t', 'nw') != 0
            "find spaces that arent used as alignment in the first indent column
            let l:spaces = search('^ \{' . &tabstop . ',}[^\t]', 'nw') != 0
            if l:tabs && l:spaces
              let b:statusline_tab_warning = '^ indent'
            elseif (l:spaces && !&expandtab) || (l:tabs && &expandtab)
              let b:statusline_tab_warning = '&expandtab'
            endif
          endif
        endif
        return b:statusline_tab_warning
      endfunction

      " trailing spaces warning
      function! Spaces()
        " return 'spaces $' if trailing spaces/tabs are present
        if !exists('b:statusline_pad_warning')
          let b:statusline_pad_warning = ''
          if &modifiable
            if exists('b:prose')
              if b:prose == 0
                if search('[ \t]\+$', 'nw') != 0
                  let b:statusline_pad_warning = '■ $'
                endif
              endif
            endif
          endif
        endif
        return b:statusline_pad_warning
      endfunction

      " recalculate the tab warning flag when idle and after writing
      autocmd cursorhold,bufwritepost * unlet! b:statusline_tab_warning
      " recalculate the trailing whitespace warning when idle, and after saving
      autocmd cursorhold,bufwritepost * unlet! b:statusline_pad_warning

    " ......................................................... Statusbar format

      " toggle vimwiki word count in statusline (0) current buffer (1) all buffers
      " or
      " toggle coding line statistics
      function! ToggleStatus(persistence)
        " show/hide word count info
        if &filetype =~ g:goyotypes
          if a:persistence == 0
            let &laststatus = (&laststatus == 0 ? 2 : 0)
          else
            let g:wikistatus = (g:wikistatus == 0 ? 2 : 0)
            let &laststatus = g:wikistatus
          endif
          " turn off persistence whenever statusline turned off :-)
          if &laststatus == 0
            let g:wikistatus = 0
          endif
        " show/hide line statistics
        else
          " toggle line info statistics where word counts are inapplicable
          if b:prose == 0
            let b:code = (b:code == 0 ? 1 : 0)
          endif
        endif
        " clear show message
        echo ''
      endfunction

      imap <F10>   <C-o>:call ToggleStatus(0)<CR>
      nmap <F10>   :call ToggleStatus(0)<CR>
      imap <S-F10> <C-o>:call ToggleStatus(1)<CR>
      nmap <S-F10> :call ToggleStatus(1)<CR>

" statusline.vim
