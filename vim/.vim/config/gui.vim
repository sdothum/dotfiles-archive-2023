" sdothum - 2016 (c) wtfpppl

" GUI
" ══════════════════════════════════════════════════════════════════════════════

  " Behaviour __________________________________________________________________

    " .................................................................... Setup

      augroup gui | autocmd! | augroup END

      " let g:gui = has('gui_running') ? 1 : 0  " gvim or console

      let g:ruler             = 0       " colorcolumn mode, see theme.vim
      let g:wrap_highlighting = 0       " wrap highlighting (0) off (1) on

    " ............................................................... Toggle gui

      nnoremap <silent><S-F12> :ToggleGui<CR>
      inoremap <silent><S-F12> <C-o>:ToggleGui<CR>
      vnoremap <silent><S-F12> :<C-u>ToggleGui<CR>

    " ............................................................... Redraw gui
      
      if $DISPLAY > ''  " initial refresh to fill window (correct status line position)
        autocmd gui VimEnter * RedrawGui
      endif

      nnoremap <silent><F12> :RedrawGui<CR>
      inoremap <silent><F12> <C-o>:RedrawGui<CR>
      vnoremap <silent><F12> :<C-u>RedrawGui<CR>

  " Display ____________________________________________________________________

    " ................................................................... Screen

      set gcr=a:blinkon0          " disable cursor blink
      set mousehide               " hide mouse when typing
      set t_Co=256                " 256 color support
      set viewoptions=folds,options,cursor,unix,slash
      set virtualedit=block       " allow virtual editing in Visual block mode
      " set virtualedit=onemore   " allow for cursor beyond last character
      set winminheight=0          " windows can be 0 line high
      set wrap                    " wrap lines for viewing

    " ................................................................... Alerts

      set noerrorbells            " don't beep
      set shortmess+=filmnrxoOtT  " abbrev. of messages (avoids "hit enter")
      set visualbell              " no sounds

      " recover last error message
      nmap <leader>e :echo errmsg<CR>

      " clear messages after awhile to keep screen clean and distraction free!
      autocmd gui CursorHold * echo

    " ................................................................ Scrolling

      if $HOST == 'monad' | set scrolloff=3
      else                | set scrolloff=5 | endif
      let g:scrolloff = &scrolloff
      set sidescroll=1  " smooth scrolling by 1 column
      set sidescrolloff=1

      " horizontal scrolling
      noremap <C-S-Left>  zL
      noremap <C-S-Right> zH

    " ..................................................... Save cursor position

      " only works for simple :buffer actions (not plugin pane selection)
      autocmd gui BufWinLeave * let b:winview = winsaveview()
      autocmd gui BufWinEnter * if exists('b:winview') | call winrestview(b:winview) | endif

  " Terminal ___________________________________________________________________

    " ..................................................................... Font

      scriptencoding utf-8
      set encoding=utf-8      " necessary to show unicode glyphs
      set ambiwidth="double"  " for double width glyph handling

    " ................................................................... Cursor

      set cursorline          " highlight current line

      set guicursor=a:block   " mode aware cursors
      set guicursor+=o:hor50-Cursor
      set guicursor+=n:Cursor
      set guicursor+=i-ci-sm:ver25-InsertCursor
      set guicursor+=r-cr:hor15-ReplaceCursor
      set guicursor+=c:CommandCursor
      set guicursor+=v-ve:VisualCursor
      set guicursor+=a:blinkon0

    " ............................... Gvim Options (make it look like terminal!)

      set guioptions+=LlRrb  " hide scrollbars
      set guioptions-=LlRrb
      set guioptions-=m      " no menubar
      set guioptions-=T      " no toolbar

  " Look _______________________________________________________________________

    " ........................................................... Column margins

      augroup column | autocmd! | augroup END

      set colorcolumn=0  " highlight column

      nmap <silent><Bar> :ToggleColumn<CR>

    " ...................................................... Line wrap highlight

      nmap <silent><F8> :ToggleColumnWrap<CR>
      imap <silent><F8> <C-o>:ToggleColumnWrap<CR>

    " ............................................................. Line numbers

      set number
      set numberwidth=10
      set relativenumber

      " toggle relative number, line number and no numbering
      nmap <silent># :ToggleNumber<CR>

      " toggle relative line numbers
      " autocmd gui InsertEnter * set norelativenumber
      " autocmd gui InsertLeave * set relativenumber

    " ................................................... Status / command lines

      set laststatus=2  " always show status line
      set ruler         " show cursor position in status line
      set noshowcmd     " show incomplete cmds in command line
      set noshowmode    " show current mode in command line

  " Highlighting _______________________________________________________________

    " ...................................................... Syntax highlighting

      set omnifunc=syntaxcomplete#Complete
      syntax on  " turn on syntax highlighting

      " refresh highlighting on arm
      autocmd gui CursorHold * if ! Prose() && &filetype != '' | execute 'set filetype=' . &filetype | endif

    " ...................................................... White space markers

      set nolist  " display tabs and trailing spaces visually
      set listchars="tab:▸\<Space>"

      " set listchars+=trail:_
      set listchars+=trail:·
      set listchars+=nbsp:.
      set listchars+=extends:>
      set listchars+=precedes:<
      " set listchars+=eol:¬

    " ..................................................... Trailing white space

      nmap <silent><leader><Space> :ToggleWhiteSpace<CR>

" gui.vim
