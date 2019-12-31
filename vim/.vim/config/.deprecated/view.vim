" sdothum - 2016 (c) wtfpl

" Views
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  "  Distraction free modes ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let s:view       = 0                  " mixed view (filetype) handling

      augroup view
        autocmd!
      augroup END

    " ................................................................ Code view

      " source code style
      function! CodeView()
        " restore CursorLine syntax highlighting (if altered by ProseView)
        if s:view != 0
          syntax enable
        endif
        let s:view = 0
        execute 'Limelight!'
        call Theme()
        call IndentTheme()
        call LiteFix()
        execute 'highlight LineNr guifg=' . g:dfm_fg_line
        call lightline#colorscheme()
        set laststatus=2                    " turn on statusline
        set showmode
      endfunction

    " ............................................................... Prose view

      " vimwiki prose style
      function! ProseView()
        let s:view = 1
        " silent !tmux set status off
        " set numberwidth=1                 " goyo settings
        " set nonumber
        " set fillchars-=stl:.              " remove statusline fillchars '.' set by goyo.vim
        " set fillchars+=stl:\ "
        call Theme()
        call DfmWriting()
        call VimwikiLink()                  " restore vimwiki link
        execute 'highlight CursorLine gui=none guibg=' . g:dfm_bg . ' guifg=' . g:dfm_fg
        execute 'Limelight'
        set colorcolumn=0
        set foldcolumn=0
        set laststatus=0
        set noshowmode
        set scrolloff=8
        set spell
      endfunction

      " dfm writing mode (single paragraph highlight)
      function! DfmWriting()
        execute 'highlight Normal guifg=' . g:dfm_unfocused
        let s:proof = 0
        call ShowInfo(0)
      endfunction

      " toggle full document highlight
      function! ToggleProof()
        call Theme()
        if s:proof == 0
          if &filetype == 'vimwiki'
            " hack to apply more robust markdown syntax highlighting
            " note: this hack may break vimwiki navigation in the future!
            set filetype=markdown
            call ProseView()
            " force margin centering
            call Refresh()
          endif
          execute 'highlight Normal guifg=' . g:dfm_proof
          let s:proof = 1
          call ShowInfo(1)
          execute 'Limelight!'
        else
          call DfmWriting()
          execute 'Limelight'
        endif
      endfunction

  " Screen focus ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Screen display

      function! LiteType()
        call Palette()
        if Prose()
          call ProseView()
        else
          call CodeView()
        endif
      endfunction

      " intial view mode: source code or prose
      autocmd view BufEnter * call LiteType()
      autocmd view VimEnter * call LiteType()

      function! Refresh()
        if Prose()
          let lstatus     = &laststatus
          call Theme()
          let &laststatus = lstatus
        else
          call CodeView()
        endif
        call Cursor()                       " restore cursor (fullscreen toggling reverts defaults)
      endfunction

      imap <silent><F9> <C-o>:call Refresh()<CR>
      nmap <silent><F9> :call Refresh()<CR>

" views.vim
