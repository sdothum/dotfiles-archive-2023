" sdothum - 2016 (c) wtfpl

" Primitive
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let g:ruler = 0                       " colorcolumn mode

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

      function! SortNumbers(nums)
        return sort(a:nums, 'core#CompareNumber')
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

    " ............................................................ Open terminal

      " open shell session in buffer directory
      nmap <silent><leader>te :silent call system('term "vimterm" STACK')<CR>

    " ............................................................... Print file

      nmap <silent><leader>ha :silent call core#Hardcopy()<CR>:echo 'Printing..'<CR>

  " Keyboard layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Colemak-shift-dh

      " environment variable
      if $COLEMAK_DH == 'true'
        call core#Colemak()
      endif

  " GUI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................... Gvim Options (make it look like terminal!)

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

      function! RefreshGui()
        call ToggleGui()
        call ToggleGui()
      endfunction

      " Toggle Menu and Toolbar
      nnoremap <silent><F12> :call ToggleGui()<CR>
      inoremap <silent><F12> <C-o>:call ToggleGui()<CR>

    " ........................................................... Column margins

      nmap <silent><Bar>         :call core#ToggleColumn()<CR>
      nmap <silent><leader><Bar> :IndentGuidesToggle<CR>:call core#IndentTheme()<CR>

    " ............................................................. Line numbers

      nmap <silent># :call core#ToggleNumber()<CR>

    " ...................................................... White space markers

      nmap <silent><leader><Space> :call core#ToggleSpaces()<CR>

  " Buffer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Line wrap

      nmap <silent><leader><CR> :call core#ToggleWrap()<CR>

    " .............................................................. Select text

      " select paragragh
      nmap <silent><A-PageUp>   :call core#ParagraphAbove()<CR>
      nmap <silent><A-PageDown> :call core#ParagraphBelow()<CR>

  " Text ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .......................................................... Shift up / down

      " shift text up / down
      imap <silent><S-Up>   <ESC>:call core#MoveLineUp()<CR>a
      imap <silent><S-Down> <ESC>:call core#MoveLineDown()<CR>a
      nmap <silent><S-Up>   :call core#MoveLineUp()<CR>
      nmap <silent><S-Down> :call core#MoveLineDown()<CR>
      vmap <silent><S-Up>   <ESC>:call core#MoveVisualUp()<CR>
      vmap <silent><S-Down> <ESC>:call core#MoveVisualDown()<CR>

    " ......................................................... Insert line wrap

      inoremap <silent><C-Return> <C-o>:call core#InsertWrap()<CR>

    " .......................................................... Code block text

      " markup wiki code blocks
      nnoremap <silent><leader>` V:call core#CodeBlock()<CR>
      vmap     <silent><leader>` :call core#CodeBlock()<CR>

  " Filetype ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Prose filestypes

      " distraction free filetyes
      function! Prose()
        return &filetype =~ 'vimwiki\|mail\|markdown\|draft'
      endfunction

      function! Markdown()
        return &filetype =~ 'vimwiki\|markdown'
      endfunction

    " ............................................................... Modifiable

      " check filetype on open
      autocmd filetype BufNewFile,BufRead * call core#CheckFiletype()

    " ................................................................... E-mail

      " position cursor for email reply or new message, see .sup/config.yaml and bin/dcompose
      autocmd filetype Filetype mail call core#ComposeMail()

" core.vim
