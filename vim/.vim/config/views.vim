" sdothum - 2016 (c) wtfpl

" Views
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  "  Distraction free modes ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Code view

      " source code style
      function! CodeView()
        execute 'LiteDFM'
        set showmode
        set laststatus=2                    " turn on statusline
        call LiteBackground()
        execute 'highlight LineNr guifg='         . g:dfm_fg_line
        " execute 'highlight CursorLineNr guibg=' . g:dfm_bg_line
        execute 'highlight CursorLineNr guibg='   . g:dfm_bg
        call Cursor()
      endfunction

    " ............................................................... Prose view

      " vimwiki prose style
      function! ProseView()
        " initialize goyo margins etc.
        call s:GoyoEnter()
        call LiteBackground()
        call HiLite()
        " hide line numbers
        execute 'highlight Normal guifg=' . g:dfm_unfocused
        execute 'highlight CursorLineNr guifg=' . g:dfm_bg . ' guibg=' . g:dfm_bg
        let s:unfocused = g:dfm_unfocused
        call Cursor()
        execute 'Limelight'
      endfunction

      function! LiteType()
        call SetTheme()
        if &filetype =~ g:goyotypes
          call ProseView()
          call DfmWriting()
        else
          call CodeView()
        endif
      endfunction

      " intial view mode: source code or prose
      autocmd BufEnter * call LiteType()

  " Goyo ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .......................................................... Goyo prose view

      " goyo initialization hooks
      " undo lite-dfm settings
      function! s:GoyoEnter()
        " silent !tmux set status off
        set scrolloff=999
        set numberwidth=1
        set foldcolumn=0
        set nonumber
        set colorcolumn=0
        set noshowmode
        if !exists('g:wikistatus')
          let g:wikistatus=1
        endif
        call DfmWriting()
        call ShowInfo()
      endfunction

      " reset vimwiki link color
      function! s:GoyoLeave()
        execute 'Limelight!'
        " silent !tmux set status on
        let &scrolloff = g:scrolloff
        set number
        " restore vimwiki link
        call VimWikiLink()
      endfunction

      autocmd User GoyoEnter nested call <SID>GoyoEnter()
      autocmd User GoyoLeave nested call <SID>GoyoLeave()

      " toggle goyo / litedfm
      function! ToggleGoyo()
        if &filetype =~ g:goyotypes
          " goyo launched yet?
          if !exists('#goyo')
            execute 'LiteDFMClose'
            " width must be greater than textwidth, center vertical (to accomodate statusline)
            execute 'Goyo ' . (&textwidth + 1) . '+1x+1'
          else
            " goyo! always returns to first buffer, so remember last
            let l:buffer = bufnr('%')
            execute 'Goyo!'
            " turn on status when not in goyo view
            call ProseView()
            call ToggleStatus()
            execute 'buffer ' . l:buffer
          endif
          " force spellcheck as autocmd sequences don't seem to set this consistently
          set spell
        endif
      endfunction

      " reset window margins by toggling goyo on and off (<C-w>= leaves number artifacts)
      function! ResetGoyo()
        if exists('#goyo')
          " goyo! always returns to first buffer, so remember last
          let l:buffer = bufnr('%')
          call ToggleGoyo()
          execute 'buffer ' . l:buffer
          call ToggleGoyo()
        endif
      endfunction

      " with window resizing, goyo margins are newly calculated
      autocmd VimResized * if &filetype =~ g:goyotypes | call ResetGoyo() | endif

  " Screen focus ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................... Screen display mode

      function! Cursor()
        execute 'highlight Cursor guibg=' . g:dfm_cursor . ' guifg=' . g:dfm_bg
      endfunction

      function! DfmWriting()
        execute 'highlight Normal guifg=' . g:dfm_unfocused
        call CursorLine(g:dfm_fg, g:dfm_bg, g:dfm_bg)
        let s:unfocused = g:dfm_unfocused
        execute 'Limelight'
        call ShowInfo()
      endfunction

      function! ToggleProof()
        " toggle between writing and proofing modes
        if &filetype =~ g:goyotypes
          if !exists('s:unfocused')
            let s:unfocused = g:dfm_unfocused
          endif
          if s:unfocused == g:dfm_unfocused
            execute 'Limelight!'
            execute 'highlight Normal guifg=' . g:dfm_proof
            call CursorLine(g:dfm_proof, g:dfm_bg, g:dfm_bg)
            let s:unfocused = g:dfm_fg
            call HideInfo()
          else
            call DfmWriting()
          end
          call HiLite()
        endif
        " restore cursor (fullscreen toggling reverts defaults)
        call Cursor()
      endfunction

      imap <silent><F11> <C-o>:call ToggleProof()<CR>
      nmap <silent><F11> :call ToggleProof()<CR>

" views.vim
