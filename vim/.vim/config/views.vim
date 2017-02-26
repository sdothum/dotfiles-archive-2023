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
        " suppress tty ctermfg error messages
        call Margin()
        call Quietly('LiteDFM')
        set showmode
        set laststatus=2                    " turn on statusline
        call LiteBackground()
        execute 'highlight LineNr guifg='         . g:dfm_fg_line
        execute 'highlight CursorLineNr guibg='   . g:dfm_bg
        call Cursor()
      endfunction

      function! Cursor()
        execute 'highlight Cursor guibg=' . g:dfm_cursor . ' guifg=' . g:dfm_bg
      endfunction

    " ............................................................... Prose view

      " vimwiki prose style
      function! ProseView()
        " silent !tmux set status off
        set scrolloff=8
        set foldcolumn=0
        set colorcolumn=0
        set noshowmode
        set spell
        " set numberwidth=1                 " goyo settings
        " set nonumber
        " set fillchars-=stl:.              " remove statusline fillchars '.' set by goyo.vim
        " set fillchars+=stl:\ "
        call DfmWriting()
        call LiteBackground()
        call HiLite()
        execute 'highlight Normal guifg='       . g:dfm_unfocused
        execute 'highlight PreProc guifg='      . g:dfm_code
        let s:unfocused = g:dfm_unfocused
        call Margin()
        call Quietly('LiteDFM')
        call Cursor()
        execute 'Limelight'
      endfunction

      " dfm writing mode (single paragraph highlight)
      function! DfmWriting()
        execute 'highlight Normal guifg=' . g:dfm_unfocused
        call CursorLine(g:dfm_fg, g:dfm_bg, g:dfm_bg)
        let s:unfocused = g:dfm_unfocused
        execute 'Limelight'
        call ShowInfo(0)
      endfunction

      " toggle full document highlight
      function! ToggleProof()
        call Margin()
        call Quietly('LiteDFM')
        call HiLite()
        if s:unfocused == g:dfm_unfocused
          execute 'Limelight!'
          execute 'highlight Normal guifg=' . g:dfm_proof
          let s:unfocused = g:dfm_fg
          call ShowInfo(1)
        else
          call DfmWriting()
        end
      endfunction

  " Screen focus ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Screen display

      function! LiteType()
        call SetTheme()
        if ProseFT()
          call ProseView()
          set laststatus=0
        else
          call CodeView()
        endif
      endfunction

      " intial view mode: source code or prose
      autocmd view     BufEnter * call LiteType()
      autocmd filetype VimEnter * call LiteType()

      function! Refresh()
        if ProseFT()
          let lstatus = &laststatus
          call Margin()
          call Quietly('LiteDFM')
          let &laststatus = lstatus
        else
          call CodeView()
        endif
        call Cursor()                       " restore cursor (fullscreen toggling reverts defaults)
      endfunction

      imap <silent><F11> <C-o>:call Refresh()<CR>
      nmap <silent><F11> :call Refresh()<CR>

" views.vim
