" sdothum - 2016 (c) wtfpppl

" GUI
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Behaviour ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup gui
        autocmd!
      augroup END

      let g:ruler     = 0                   " colorcolumn mode, see theme.vim
      let s:wraplight = 0                   " highlight linewrap (0) off (1) on
      let s:breakchar = '→'
      let s:delay     = '200m'              " redraw delay, see theme#FontSize()

    " ............................................................... Toggle gui

      " toggle gui menu
      function! s:toggleGui()
        if &guioptions =~# 'T'
          set guioptions-=T
          set guioptions-=m
        else
          set guioptions+=T
          set guioptions+=m
        endif
      endfunction

      nnoremap <silent><S-F12> :call <SID>toggleGui()<CR>
      inoremap <silent><S-F12> <C-o>:call <SID>toggleGui()<CR>
      vnoremap <silent><S-F12> :<C-u>call <SID>toggleGui()<CR>

    " ............................................................... Redraw gui

      " toggle in/out to fill window
      function! s:redrawGui()
        call <SID>toggleGui()
        execute 'sleep ' . s:delay
        call <SID>toggleGui()
        " fix line wrap highlighting
        Quietly Retheme
      endfunction
      
      command! RedrawGui call <SID>redrawGui()

      " initial refresh to fill window (correct status line position)
      if $DISPLAY > ''
        autocmd gui VimEnter * RedrawGui
      endif

      nnoremap <silent><F12> :call <SID>redrawGui()<CR>
      inoremap <silent><F12> <C-o>:call <SID>redrawGui()<CR>
      vnoremap <silent><F12> :<C-u>call <SID>redrawGui()<CR>

  " Display ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................... Screen

      set gcr=a:blinkon0                    " disable cursor blink
      set mousehide                         " hide mouse when typing
      set t_Co=256                          " 256 color support
      set viewoptions=folds,options,cursor,unix,slash
      set virtualedit=block                 " allow virtual editing in Visual block mode
      " set virtualedit=onemore             " allow for cursor beyond last character
      set winminheight=0                    " windows can be 0 line high
      set wrap                              " wrap lines for viewing

    " ................................................................... Alerts

      set noerrorbells                      " don't beep
      set shortmess+=filmnrxoOtT            " abbrev. of messages (avoids "hit enter")
      set visualbell                        " no sounds

      " recover last error message
      nmap <leader>e :echo errmsg<CR>

      " clear messages after awhile to keep screen clean and distraction free!
      autocmd gui CursorHold * echo

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
      " horizontal scrolling
      noremap <C-S-Left>  zL
      noremap <C-S-Right> zH

    " ..................................................... Save cursor position

      " only works for simple :buffer actions (not plugin pane selection)
      autocmd gui BufWinLeave * let b:winview = winsaveview()
      autocmd gui BufWinEnter * if exists('b:winview') | call winrestview(b:winview) | endif

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

      augroup column
        autocmd!
      augroup END

      set colorcolumn=0                     " highlight column

      " toggle colorcolumn modes, see theme#IndentTheme()
      function! s:toggleColumn()
        if g:ruler == 0
          let g:ruler      = 1
          let &colorcolumn = col('.') 
          autocmd column CursorMoved,CursorMovedI * let &colorcolumn = col('.')
        elseif g:ruler == 1
          let g:ruler      = 2
          autocmd! column
        else
          let g:ruler      = 0
          let &colorcolumn = 0
          ColumnWrap
        endif
        call theme#IndentTheme()
        let g:column = 1                    " flash column position, see autocmd info.vim
      endfunction

      nmap <silent><Bar>  :call <SID>toggleColumn()<CR>

    " ...................................................... Line wrap highlight

      " highlight wrapped line portion, see theme#Theme()
      function! s:columnWrap()
        if g:ruler == 0 && s:wraplight
          set showbreak=
          let l:edge       = winwidth(0) - &numberwidth - &foldcolumn - 1
          let &colorcolumn = join(range(l:edge, 999), ',')
        else
          execute 'set showbreak=' . s:breakchar . '\ '
        endif
      endfunction

      function! s:toggleColumnWrap(...)
        let s:wraplight = a:0 ? a:1 : (s:wraplight ? 0 : 1)
        let g:ruler     = -1
        call <SID>toggleColumn()
      endfunction

      command!          ColumnWrap       call <SID>columnWrap()
      command! -nargs=? ToggleColumnWrap call <SID>toggleColumnWrap(<f-args>)

      nmap <silent><F8> :ToggleColumnWrap<CR>
      imap <silent><F8> <C-o>:ToggleColumnWrap<CR>

    " ............................................................. Line numbers

      set number
      set numberwidth=10
      set relativenumber

      " toggle relative line numbers
      " autocmd gui InsertEnter * set norelativenumber
      " autocmd gui InsertLeave * set relativenumber

      " toggle relative number, line number and no numbering
      function! s:toggleNumber()
        if (&relativenumber == 1 && &number == 1)
          set norelativenumber
        elseif (&relativenumber == 0 && &number == 1)
          set nonumber
        else
          set relativenumber
          set number
        endif
      endfunction

      nmap <silent># :call <SID>toggleNumber()<CR>

    " ................................................... Status / command lines

      set laststatus=2                      " always show status line
      set ruler                             " show cursor position in status line
      set noshowcmd                         " show incomplete cmds in command line
      set noshowmode                        " show current mode in command line

  " Highlighting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................... Syntax highlighting

      set omnifunc=syntaxcomplete#Complete
      syntax on                             " turn on syntax highlighting
      " refresh highlighting on arm
      autocmd gui CursorHold * if !core#Prose() && &filetype != '' | execute 'set filetype=' . &filetype | endif

    " ...................................................... White space markers

      set nolist                            " display tabs and trailing spaces visually
      set listchars="tab:▸\<Space>"

      " set listchars+=trail:_
      set listchars+=trail:·
      set listchars+=nbsp:.
      set listchars+=extends:>
      set listchars+=precedes:<
      " set listchars+=eol:¬

    " .......................................... White space / soft wrap markers

      augroup soft
        autocmd!
      augroup END

      " soft wrap marker
      function! s:softMark()
        " filetype dependent textwidth
        if exists('s:soft')
          call matchdelete(s:soft)
        endif
        highlight SoftWrap cterm=underline gui=underline
        let s:soft = '\%' . (&textwidth + 1) . 'v'
        let s:soft = matchadd('SoftWrap', s:soft)
      endfunction

      " toggle trailing whitespace highlight and indentation levels
      function! s:toggleSpaces()
        set list!
        if &list == 0
          match ExtraWhitespace /\%x00$/    " nolist by failing match with null character :)
          call matchdelete(s:soft)
          unlet s:soft
          autocmd! soft
          " echo ''
        else
          match ExtraWhitespace /\s\+$/
          call <SID>softMark()
          autocmd soft BufEnter * call <SID>softMark()
          " echo 'List invisibles ON'
        endif
      endfunction

      nmap <silent><leader><Space> :call <SID>toggleSpaces()<CR>

" gui.vim
