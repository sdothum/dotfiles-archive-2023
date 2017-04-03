" sdothum - 2016 (c) wtfpl

" Views
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  "  Distraction free modes ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup view
        autocmd!
      augroup END

    " ................................................................ Code view

      " source code style
      function! CodeView()
        " reset theme colours when transitioning from prose view
        call lightline#colorscheme()
        call Theme()
        set laststatus=2                    " turn on statusline
        set showmode
        execute 'highlight LineNr guifg=' . g:dfm_fg_line
        call IndentTheme()
        call LiteFix()
      endfunction

    " ............................................................... Prose view

      " vimwiki prose style
      function! ProseView()
        " silent !tmux set status off
        set colorcolumn=0
        set foldcolumn=0
        set laststatus=0
        set noshowmode
        set scrolloff=8
        set spell
        " set numberwidth=1                 " goyo settings
        " set nonumber
        " set fillchars-=stl:.              " remove statusline fillchars '.' set by goyo.vim
        " set fillchars+=stl:\ "
        call DfmWriting()
        call Theme()
        call VimWikiLink()                  " restore vimwiki link
        execute 'Limelight'
      endfunction

      " dfm writing mode (single paragraph highlight)
      function! DfmWriting()
        execute 'highlight Normal guifg=' . g:dfm_unfocused
        let s:unfocused = g:dfm_unfocused
        call ShowInfo(0)
      endfunction

      " toggle full document highlight
      function! ToggleProof()
        call Theme()
        if s:unfocused == g:dfm_unfocused
          execute 'highlight Normal guifg=' . g:dfm_proof
          let s:unfocused = g:dfm_proof
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
          let lstatus = &laststatus
          call Theme()
          let &laststatus = lstatus
        else
          call CodeView()
        endif
        call Cursor()                       " restore cursor (fullscreen toggling reverts defaults)
      endfunction

      imap <silent><F9> <C-o>:call Refresh()<CR>
      nmap <silent><F9>      :call Refresh()<CR>

" views.vim
