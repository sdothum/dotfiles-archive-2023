" sdothum - 2016 (c) wtfpppl

" GUI
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Behaviour ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup gui
        autocmd!
      augroup END

    " ................................................................... Alerts

      set noerrorbells                      " don't beep
      set shortmess+=filmnrxoOtT            " abbrev. of messages (avoids "hit enter")
      set visualbell                        " no sounds

      " recover last error message
      nmap <leader>e :echo errmsg<CR>

      " clear messages after awhile to keep screen clean and distraction free!
      autocmd gui CursorHold * echo

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

      set guicursor=a:block                 " mode aware cursors
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

  " Look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Column margins

      set colorcolumn=0                     " highlight column

    " ............................................................. Line numbers

      set number
      set numberwidth=10
      set relativenumber

      " toggle relative line numbers
      " autocmd gui InsertEnter * set norelativenumber
      " autocmd gui InsertLeave * set relativenumber

    " .................................................................... Theme

      " reset menu highlight after loading autocompletion plugin
      autocmd gui BufWinEnter * highlight PmenuSel term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=#b58900 guibg=#fdf6e3
      " match command line tab menu
      autocmd gui BufWinEnter * highlight WildMenu term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=#b58900 guibg=#fdf6e3
      " toggle in/out to fill window
      autocmd gui VimEnter    * call RefreshGui()

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

" gui.vim
