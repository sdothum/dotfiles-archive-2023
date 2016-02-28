" sdothum - 2016 (c) wtfpl

" Themes
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Solarized

      " further distraction free mode settings
      " foreground
      let g:dfm_fg = '#002b36'              " dark foreground
      let g:dfm_fg_dark = '#fdf6e3'         " light foreground
      let g:dfm_proof = '#073642'           " dark foreground
      let g:dfm_proof_dark = '#eee8d5'      " light foreground
      let g:dfm_unfocused = '#93a1a1'       " light grey surrounding text content
      let g:dfm_unfocused_dark = '#576565'  " dark grey surrounding text content

      " background
      let g:dfm_bg = '#fdf6e3'              " solarized light (paper) background
      let g:dfm_bg_dark = '#002b36'         " solarized dark background

      " cursor line
      let g:dfm_cursor = '#54D4FF'          " ia writer blue cursor
      let g:dfm_cursor_dark = '#DA4716'     " reddish cursor
      let g:dfm_bg_line = '#eee8d5'         " solarized light cursorline
      let g:dfm_bg_line_dark = '#073642'    " solarized dark cursorline
      let g:dfm_fg_line = '#cccccc'         " light grey line numbers
      let g:dfm_fg_line_dark = '#444444'    " dark grey line numbers

      " match marks margin and whitespace colours to background
      function! LiteBackground()
        if &background == 'light'
          execute 'highlight ShowMarksHLl    guibg=' . g:dfm_bg
          execute 'highlight SignColumn      guibg=' . g:dfm_bg
          execute 'highlight ExtraWhitespace guibg=' . g:dfm_cursor_dark . ' guifg=' . g:dfm_bg_dark
          execute 'highlight InsertCursor    guibg=' . g:dfm_cursor      . ' guifg=' . g:dfm_bg
          execute 'highlight VisualCursor    guibg=' . g:dfm_cursor_dark . ' guifg=' . g:dfm_bg
          execute 'highlight ReplaceCursor   guibg=' . g:dfm_cursor_dark . ' guifg=' . g:dfm_bg
          execute 'highlight CommandCursor   guibg=' . g:dfm_cursor_dark . ' guifg=' . g:dfm_bg
        else
          execute 'highlight ShowMarksHLl    guibg=' . g:dfm_bg_dark
          execute 'highlight SignColumn      guibg=' . g:dfm_bg_dark
          execute 'highlight ExtraWhitespace guibg=' . g:dfm_cursor      . ' guifg=' . g:dfm_bg
          execute 'highlight InsertCursor    guibg=' . g:dfm_cursor_dark . ' guifg=' . g:dfm_bg
          execute 'highlight VisualCursor    guibg=' . g:dfm_cursor      . ' guifg=' . g:dfm_bg
          execute 'highlight ReplaceCursor   guibg=' . g:dfm_cursor      . ' guifg=' . g:dfm_bg
          execute 'highlight CommandCursor   guibg=' . g:dfm_cursor      . ' guifg=' . g:dfm_bg
        end
      endfunction

    " ............................................................ Switch colour

      " restore vimwiki link
      function! VimWikiLink()
        highlight VimwikiLink guifg=#268bd2 gui=bold
      endfunction

      " toggle colour scheme
      function! LiteSwitch()
        " 1st usage causes one time initialization error, trap error instead
        call Quietly('LiteDFMClose')
        let &background = (&background == 'dark' ? 'light' : 'dark')
        call LiteType()
        if exists('#goyo')
          call ToggleHiLite()
        else
          " match lightline to current colorscheme
          " see https://github.com/itchyny/lightline.vim/issues/104
          if &background == 'light'
            let g:lightline.colorscheme = 'solarized_light'
          else
            let g:lightline.colorscheme = 'solarized_dark'
          end
          call lightline#init()
          call lightline#colorscheme()
          call lightline#update()
        endif
        " restore vimwiki link
        call VimWikiLink()
      endfunction

      imap <silent><F9> <C-o>:call LiteSwitch()<CR>
      nmap <silent><F9> :call LiteSwitch()<CR>

  " Cursor ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Cursorline

      " set prose cursorline theme
      function! HiLite()
        if !exists('s:cursorline')
          call ToggleHiLite()
        endif
        execute 'highlight CursorLine gui=none guibg=' . s:cursorline . ' guifg=' . s:foreground
      endfunction

      " cursorline contrast (0) low (1) high
      let s:contrast = 1

      function! CursorLine(fg, bg, BG)
        let s:foreground = a:fg
        if s:cursorline != a:bg
          let s:cursorline = a:bg
        else
          let s:cursorline = a:BG
        endif
      endfunction

      function! ToggleHiLite()
        if &filetype =~ g:goyotypes
          if !exists('s:cursorline')
            let s:cursorline = '#000000'
          endif
          if s:contrast == 0
            " low contrast cursorline
            if &background == 'light'
              " call CursorLine(g:dfm_fg, g:dfm_bg, g:dfm_bg_line)
              call CursorLine(g:dfm_fg, g:dfm_bg, g:dfm_bg)
            else
              call CursorLine(g:dfm_fg_dark, g:dfm_bg_dark, g:dfm_bg_dark)
            end
          else
            " high contrast cursorline
            if &background == 'light'
              " call CursorLine(g:dfm_fg, g:dfm_bg_line, g:dfm_bg)
              call CursorLine(g:dfm_fg, g:dfm_bg, g:dfm_bg)
            else
              call CursorLine(g:dfm_fg_dark, g:dfm_bg_dark, g:dfm_bg_dark)
            end
          endif
          call HiLite()
        endif
      endfunction

      " imap <silent><S-F9> <C-o>:call ToggleHiLite()<CR>
      " nmap <silent><S-F9> :call ToggleHiLite()<CR>

  " Font ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch font

      function! FontSwitch()
        " adjust font displays for various gpu's
        if system("lspci") =~ 'VGA .* NVIDIA'
          " for macbook nvidia gpu
          if &guifont =~ 'Input Mono'
            set guifont=PragmataPro\ 11
            execute 'set linespace=' . (argv(0) == 'thedarnedestthing' || expand('%:t') =~ '\(wiki\|eml\)$' ? 15 : 5)
          else
            set guifont=Input\ Mono\ Compressed\ 11
            execute 'set linespace=' . (argv(0) == 'thedarnedestthing' || expand('%:t') =~ '\(wiki\|eml\)$' ? 11 : 0)
          endif
        else
          " for ati/intel gpu's
          if &guifont =~ 'Input Mono'
            set guifont=PragmataPro\ 12
            execute 'set linespace=' . (argv(0) == 'thedarnedestthing' || expand('%:t') =~ '\(wiki\|eml\)$' ? 16 : 5)
          else
            set guifont=Input\ Mono\ Compressed\ 12
            execute 'set linespace=' . (argv(0) == 'thedarnedestthing' || expand('%:t') =~ '\(wiki\|eml\)$' ? 12 : 0)
          endif
        endif
        " toggle fullscreen to reposition statusline
        call system('wmctrl -ir ' . v:windowid . ' -b toggle,fullscreen')
        call system('wmctrl -ir ' . v:windowid . ' -b toggle,fullscreen')
      endfunction

      " set initial font and line spacing
      call FontSwitch()

      imap <silent><C-F9> <C-o>:call FontSwitch()<CR>
      nmap <silent><C-F9> :call FontSwitch()<CR>

" themes.vim
