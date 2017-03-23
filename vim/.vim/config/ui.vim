" sdothum - 2016 (c) wtfpppl

" UI
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Behaviour ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup ui
        autocmd!
      augroup END

      augroup column
        autocmd!
      augroup END

    " ................................................................... Alerts

      set noerrorbells                      " don't beep
      set shortmess+=filmnrxoOtT            " abbrev. of messages (avoids "hit enter")
      set visualbell                        " no sounds

      " recover last error message
      nmap <leader>e :echo errmsg<CR>

      " clear messages after awhile to keep screen clean and distraction free!
      autocmd setup cursorhold * echo

    " .................................................................. Display

      set gcr=a:blinkon0                    " disable cursor blink
      set mousehide                         " hide mouse when typing
      set t_Co=256                          " 256 color support
      set viewoptions=folds,options,cursor,unix,slash
      set virtualedit=block                 " allow virtual editing in Visual block mode
      " set virtualedit=onemore             " allow for cursor beyond last character
      set winminheight=0                    " windows can be 0 line high
      set wrap                              " wrap lines for viewing

    " ................................................................ Scrolling

      " set scrolljump=8                    " lines to scroll when cursor leaves screen
      if $HOST == 'monad'
        set scrolloff=3
      else
        set scrolloff=5
      endif
      let g:scrolloff = &scrolloff
      set sidescroll=1                      " smooth scrolling by 1 column
      set sidescrolloff=1
      " easier horizontal scrolling
      noremap <C-Left>       zL
      noremap <C-Right>      zH

      " space now commandeered as leader by spacemacs wannabe :-)
      " " manual jump scrolling
      " if $HOST == 'monad'
      "   nnoremap <Space>   10jzz
      "   nnoremap <S-Space> 10kzz
      " else
      "   nnoremap <Space>   30jzz
      "   nnoremap <S-Space> 30kzz
      " endif

  " Terminal ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ..................................................................... Font

      scriptencoding utf-8
      set encoding=utf-8                    " necessary to show unicode glyphs

    " ................................................................... Cursor

      set cursorline                        " highlight current line

      set guicursor=a:block                 " mode aware cursors, see solarized functions.vim
      set guicursor+=o:hor50-Cursor
      set guicursor+=n:Cursor
      set guicursor+=i-ci-sm:ver25-InsertCursor
      set guicursor+=r-cr:hor15-ReplaceCursor
      set guicursor+=c:CommandCursor
      set guicursor+=v-ve:VisualCursor
      set guicursor+=a:blinkon0

    " ............................... Gvim Options (make it look like terminal!)


      set guioptions+=LlRrb                 " hide scrollbars
      set guioptions-=LlRrb
      set guioptions-=m                     " no menubar
      set guioptions-=T                     " no toolbar

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

      " Toggle Menu and Toolbar
      nnoremap <silent><F12> :call ToggleGui()<CR>
      inoremap <silent><F12> <C-o>:call ToggleGui()<CR>

  " Look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Column margins

      set colorcolumn=0                     " highlight column

      " see plugins.vim IndentTheme()
      let g:ruler = 0

      " toggle colorcolumn modes
      function! ToggleColumn()
        if g:ruler == 0
          let g:ruler = 1
          let &colorcolumn = col('.')
          autocmd column CursorMoved,CursorMovedI * let &colorcolumn = col('.')
        else
          if g:ruler == 1
            let g:ruler = 2
            autocmd! column
          else
            let g:ruler = 0
            let &colorcolumn = 0
          endif
        endif
        call IndentTheme()
      endfunction

      nmap <silent><Bar>      :call ToggleColumn()<CR>
      nmap <silent><Bar><Bar> :IndentGuidesToggle<CR>:call IndentTheme()<CR>

    " ............................................................. Line numbers

      set number                            " line numbers are good
      set numberwidth=10
      set number

      " toggle relative number, line number and no numbering
      function! ToggleNumber()
        if (&relativenumber == 1 && &number == 1)
          set norelativenumber
        else
          if (&relativenumber == 0 && &number == 1)
            set nonumber
          else
            set relativenumber
            set number
          endif
        endif
      endfunction

      nmap <silent># :call ToggleNumber()<CR>

      " toggle relative line numbers
      autocmd ui InsertEnter * set relativenumber
      autocmd ui InsertLeave * set norelativenumber

    " ................................................... Status / command lines

      " see Lightline plugins.vim
      set laststatus=2                      " always show status line
      set ruler                             " show cursor position in status line
      set noshowcmd                         " show incomplete cmds in command line
      set noshowmode                        " show current mode in command line

  " Highlighting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................... Syntax highlighting

      set omnifunc=syntaxcomplete#Complete
      syntax on                             " turn on syntax highlighting

    " ...................................................... White space markers

      set nolist                            " display tabs and trailing spaces visually
      set listchars="tab:▸\<Space>"

      " set listchars+=trail:_
      set listchars+=trail:·
      set listchars+=nbsp:.
      set listchars+=extends:>
      set listchars+=precedes:<
      " set listchars+=eol:¬

      " toggle trailing whitespace highlight and indentation levels
      function! ToggleSpaces()
        set list!
        if &list == 0
          match ExtraWhitespace /\%x00$/    " nolist by failing match with null character :-)
          " echo ''
          let g:matchspace = ''
        else
          match ExtraWhitespace /\s\+$/
          " echo 'List invisibles ON'
          let g:matchspace = '■'
        endif
      endfunction

      nmap <silent><leader><Space> :call ToggleSpaces()<CR>

" ui.vim
