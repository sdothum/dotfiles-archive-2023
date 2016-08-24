" sdothum - 2016 (c) wtfpl

" Views
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  "  Distraction free modes ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Code view

      " source code style
      function! CodeView()
        " suppress tty ctermfg error messages
        call Quietly('LiteDFM')
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
        call s:GoyoEnter()
        call LiteBackground()
        call HiLite()
        execute 'highlight Normal guifg=' . g:dfm_unfocused
        execute 'highlight CursorLineNr guifg=' . g:dfm_bg . ' guibg=' . g:dfm_bg
        execute 'highlight PreProc guifg=' . g:dfm_code
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

      let g:wikistatus = 1                  " initial wikistatus, see statusline.vim
      let g:goyorefresh = 0                 " goyo refresh (0) pending (1) done

      " goyo initialization hooks
      function! s:GoyoEnter()
        " silent !tmux set status off
        set scrolloff=8
        set numberwidth=1
        set foldcolumn=0
        set nonumber
        set colorcolumn=0
        set noshowmode
        set fillchars-=stl:.                " remove statusline fillchars '.' set by goyo.vim
        " set fillchars+=stl:\ "
        call DfmWriting()
      endfunction

      " reset vimwiki link color
      function! s:GoyoLeave()
        execute 'Limelight!'
        let &scrolloff = g:scrolloff        " silent !tmux set status on
        set number
        call VimWikiLink()                  " restore vimwiki link
      endfunction

      autocmd User GoyoEnter nested call <SID>GoyoEnter()
      autocmd User GoyoLeave nested call <SID>GoyoLeave()

      " toggle goyo / litedfm
      function! ToggleGoyo()
        if &filetype =~ g:goyotypes
          if !exists('#goyo')               " goyo launched yet?
            execute 'LiteDFMClose'
            " width must be greater than textwidth, center vertical (to accomodate statusline)
            execute 'Goyo ' . (&textwidth + 1) . '+1x+0'
          else
            let l:buffer = bufnr('%')       " goyo! always returns to first buffer, so remember last
            execute 'Goyo!'
            call ProseView()                " turn on status when not in goyo view
            execute 'buffer ' . l:buffer
          endif
          set spell                         " force spellcheck
        endif
      endfunction

      " reset window margins by toggling goyo on and off (<C-w>= leaves number artifacts)
      function! ResetGoyo()
        let g:goyorefresh=1                 " prevent window manager resize loop
        if exists('#goyo')
          let l:buffer = bufnr('%')         " goyo! always returns to first buffer, so remember last
          call ToggleGoyo()
          execute 'buffer ' . l:buffer
          call ToggleGoyo()
        endif
      endfunction

      " with window resizing, goyo margins are newly calculated
      autocmd VimResized * if &filetype =~ g:goyotypes && g:goyorefresh == 0 | call ResetGoyo() | endif
      " reset refresh indicator after awhile
      autocmd cursorhold * let g:goyorefresh = 0

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
        call ShowInfo(0)
      endfunction

      function! ToggleProof()
        if &filetype =~ g:goyotypes         " toggle between writing and proofing modes
          if s:unfocused == g:dfm_unfocused
            execute 'Limelight!'
            execute 'highlight Normal guifg=' . g:dfm_proof
            " call CursorLine(g:dfm_proof, g:dfm_bg, g:dfm_bg)
            let s:unfocused = g:dfm_fg
            call ShowInfo(1)
          else
            call DfmWriting()
          end
          call HiLite()
        endif
        call Cursor()                       " restore cursor (fullscreen toggling reverts defaults)
      endfunction

      imap <silent><F11> <C-o>:call ToggleProof()<CR>
      nmap <silent><F11> :call ToggleProof()<CR>

" views.vim
