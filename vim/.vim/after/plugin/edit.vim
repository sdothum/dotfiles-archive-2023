" sdothum - 2016 (c) wtfpl

" Edit
" ══════════════════════════════════════════════════════════════════════════════

  " Line _______________________________________________________________________

    " .............................................................. Insert line

      " insert line while disabling auto-commenting OR break (prose) line
      function! s:smartWrap()
        if Prose()  " override Pencil mode (the default state for prose)
          set paste
          execute "normal! i\<CR>"
          set nopaste
          execute 'startinsert'
        else  " append EOL wrap from any col position
          let l:formatoptions = &formatoptions  " disable auto commenting
          set formatoptions-=c
          set formatoptions-=r
          set formatoptions-=o
          normal! ^
          let l:pos = col('.')
          normal! o
          " align line indentation
          execute 'normal! a' . repeat(' ', l:pos)
          let &formatoptions = l:formatoptions
        endif
      endfunction

      command! SmartWrap silent! call <SID>smartWrap()

    " ......................................................... Strip whitespace

      " strips trailing whitespace from all lines, see https://dougblack.io/words/a-good-vimrc.html
      function! s:stripTrailingWhitespaces()
        if &modifiable == 1 && ! s:markdown()
          " let l:_s = @/ " save last search & cursor position
          " let l:l  = line(".")
          " let l:c  = col(".")
          let s:view = winsaveview()
          %s/\s\+$//e           " EOL
          %s/\(\n\r\?\)\+\%$//e " EOF
          call winrestview(s:view)
          " let @/ = l:_s
          " call cursor(l:l, l:c)
        endif
      endfunction

      command! StripTrailingWhitespaces silent! call <SID>stripTrailingWhitespaces()

    " ............................................................. Convert tabs

      :command! -range=% -nargs=0 Tab2Space execute '<line1>,<line2>s#^\t\+#\=repeat(" ", len(submatch(0))*' . &ts . ')'
      :command! -range=% -nargs=0 Space2Tab execute '<line1>,<line2>s#^\( \{'.&ts.'\}\)\+#\=repeat("\t", len(submatch(0))/' . &ts . ')'

    " .......................................................... Code block text

      " convert wiki text lines into code block lines
      function! s:codeBlock()
        execute "silent! normal  :s/\\(.*\\)/`\\1`/\<CR>"
        " preserve leading spaces with wiki markdown
        execute "silent! normal! gv:s/^` /`^ /\<CR>"
        execute "silent! normal! gv:s/^``/`^ `/e\<CR>"
        " convert [[test]], see thedarnedestthing markdown
        execute "silent! normal! gv:s/ \\[\\[ / [[] /e\<CR>"
        execute "silent! normal! gv:s/ \\]\\] / []] /e\<CR>"
      endfunction

      command! CodeBlock silent! call <SID>codeBlock()

" edit.vim
