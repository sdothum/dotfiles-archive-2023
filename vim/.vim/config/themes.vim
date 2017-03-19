" sdothum - 2016 (c) wtfpl

" Themes
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

      augroup theme
        autocmd!
      augroup END

    " .......................................................... Text and cursor

      " further distraction free mode settings
      " foreground
      let g:dfm_fg_light        = 'brown'   " light foreground
      let g:dfm_fg_dark         = '#FFFF00' " dark foreground
      let g:dfm_fg_dark_prose   = '#54D4FF' " dark foreground blue
      let g:dfm_proof_light     = '#002b36' " dark foreground
      let g:dfm_proof_dark      = '#fdf6e3' " light foreground
      let g:dfm_unfocused_light = '#002b36' " light grey surrounding text content
      let g:dfm_unfocused_dark  = '#fdf6e3' " dark grey surrounding text content
      let g:dfm_code_light      = '#859900' " light code foreground
      let g:dfm_code_dark       = '#859900' " dark code foreground

      " background
      let g:dfm_bg_light        = '#fdf6e3' " solarized light (paper) background
      let g:dfm_bg_dark         = '#002b36' " solarized dark background

      " cursor line
      let g:dfm_cursor_light    = '#54D4FF' " ia writer blue cursor
      let g:dfm_cursor_dark     = '#DA4716' " reddish cursor
      let g:dfm_bg_line_light   = '#eee8d5' " solarized light cursorline
      let g:dfm_bg_column_light = '#E2D7B6' " solarized light column
      let g:dfm_bg_line_dark    = '#073642' " solarized dark cursorline
      let g:dfm_bg_column_dark  = '#0A4C5C' " solarized dark column
      let g:dfm_fg_line_light   = '#cccccc' " light grey line numbers
      let g:dfm_fg_line_dark    = '#444444' " dark grey line numbers

      " statusline
      let g:dfm_status_light    = '#073642' " light statusline
      let g:dfm_status_dark     = '#eee8d5' " dark statusline

      function! SetTheme()
        if &background == 'light'
          let g:dfm_fg          = g:dfm_fg_light
          let g:dfm_proof       = g:dfm_proof_light
          let g:dfm_unfocused   = g:dfm_unfocused_light
          let g:dfm_code        = g:dfm_code_light
          let g:dfm_bg          = g:dfm_bg_light
          let g:dfm_cursor      = g:dfm_cursor_light
          let g:dfm_bg_line     = g:dfm_bg_line_light
          let g:dfm_fg_line     = g:dfm_fg_line_light
          let g:dfm_status      = g:dfm_status_light
          execute 'highlight IndentGuidesOdd guibg='  . g:dfm_bg_light
          execute 'highlight IndentGuidesEven guibg=' . g:dfm_bg_line_dark
        else
          if ProseFT()
            let g:dfm_fg        = g:dfm_fg_dark_prose
          else
            let g:dfm_fg        = g:dfm_fg_dark
          endif
          let g:dfm_proof       = g:dfm_proof_dark
          let g:dfm_unfocused   = g:dfm_unfocused_dark
          let g:dfm_code        = g:dfm_code_dark
          let g:dfm_bg          = g:dfm_bg_dark
          let g:dfm_cursor      = g:dfm_cursor_dark
          let g:dfm_bg_line     = g:dfm_bg_line_dark
          let g:dfm_fg_line     = g:dfm_fg_line_dark
          let g:dfm_status      = g:dfm_status_dark
          execute 'highlight IndentGuidesOdd guibg='  . g:dfm_bg_dark
          execute 'highlight IndentGuidesEven guibg=' . g:dfm_bg_line_dark
        endif
      endfunction

    " .................................................................. Margins

      " match marks margin and whitespace colours to background
      function! LiteBackground()
        execute 'highlight ShowMarksHLl      guibg=' . g:dfm_bg
        execute 'highlight SignColumn        guibg=' . g:dfm_bg
        execute 'highlight InsertCursor      guibg=' . g:dfm_cursor       . ' guifg=' . g:dfm_bg
        if &background == 'light'
          execute 'highlight ExtraWhitespace guibg=' . g:dfm_cursor_dark  . ' guifg=' . g:dfm_bg_dark
          execute 'highlight VisualCursor    guibg=' . g:dfm_cursor_dark  . ' guifg=' . g:dfm_bg
          execute 'highlight ReplaceCursor   guibg=' . g:dfm_cursor_dark  . ' guifg=' . g:dfm_bg
          execute 'highlight CommandCursor   guibg=' . g:dfm_cursor_dark  . ' guifg=' . g:dfm_bg
        else
          execute 'highlight ExtraWhitespace guibg=' . g:dfm_cursor_light . ' guifg=' . g:dfm_bg_light
          execute 'highlight VisualCursor    guibg=' . g:dfm_cursor_light . ' guifg=' . g:dfm_bg
          execute 'highlight ReplaceCursor   guibg=' . g:dfm_cursor_light . ' guifg=' . g:dfm_bg
          execute 'highlight CommandCursor   guibg=' . g:dfm_cursor_light . ' guifg=' . g:dfm_bg
        end
      endfunction

    " ............................................................ Switch colour

      " restore vimwiki link
      function! VimWikiLink()
        highlight VimwikiLink guifg=#268bd2 gui=bold
      endfunction

      " toggle colour scheme
      function! LiteSwitch()
        " trap and ignore initialization error
        call Quietly('LiteDFMClose')
        let &background = (&background == 'dark' ? 'light' : 'dark')
        if &background == 'light'
          colorscheme solarized8_light_high
        else
          colorscheme solarized8_dark_high
        endif
        call SetTheme()
        call LiteType()
        if ProseFT()
          call ToggleHiLite()
        else
          " match lightline to current colorscheme
          " see https://github.com/itchyny/lightline.vim/issues/104
          if &background == 'light'
            let g:lightline.colorscheme = 'solarized_light'
          else
            let g:lightline.colorscheme = 'solarized_dark'
          endif
          call lightline#init()
          call lightline#colorscheme()
          call lightline#update()
        endif
        call VimWikiLink()                  " restore vimwiki link
        call IndentTheme()
        call LiteFix()
      endfunction

      function! LiteFix()
        " fix colour shifts resulting from toggling(!)
        if &background == 'light'
          highlight LightlineLeft_normal_0    ctermfg=230 ctermbg=33  guifg=#fdf6e3 guibg=#268bd2
          highlight LightlineLeft_normal_0_1  ctermfg=33  ctermbg=244 guifg=#268bd2 guibg=#839496
          highlight LightlineLeft_normal_1    ctermfg=230 ctermbg=244 guifg=#fdf6e3 guibg=#839496
          highlight LightlineLeft_normal_1_2  ctermfg=244 ctermbg=187 guifg=#839496 guibg=#eee8d5
          highlight LightlineRight_normal_0   ctermfg=230 ctermbg=239 guifg=#fdf6e3 guibg=#586e75
          highlight LightlineRight_normal_0_1 ctermfg=239 ctermbg=244 guifg=#586e75 guibg=#839496
          highlight LightlineRight_normal_1   ctermfg=230 ctermbg=244 guifg=#fdf6e3 guibg=#839496
          highlight LightlineRight_normal_1_2 ctermfg=244 ctermbg=187 guifg=#839496 guibg=#eee8d5
        else
          highlight LightlineLeft_normal_0    ctermfg=234 ctermbg=33  guifg=#002b36 guibg=#268bd2
          highlight LightlineLeft_normal_0_1  ctermfg=33  ctermbg=240 guifg=#268bd2 guibg=#657b83
          highlight LightlineLeft_normal_1    ctermfg=234 ctermbg=240 guifg=#002b36 guibg=#657b83
          highlight LightlineLeft_normal_1_2  ctermfg=240 ctermbg=235 guifg=#657b83 guibg=#073642
          highlight LightlineRight_normal_0   ctermfg=234 ctermbg=245 guifg=#002b36 guibg=#93a1a1
          highlight LightlineRight_normal_0_1 ctermfg=245 ctermbg=240 guifg=#93a1a1 guibg=#657b83
          highlight LightlineRight_normal_1   ctermfg=234 ctermbg=240 guifg=#002b36 guibg=#657b83
          highlight LightlineRight_normal_1_2 ctermfg=240 ctermbg=235 guifg=#657b83 guibg=#073642
        endif
      endfunction

      nmap <silent><F9>         :call LiteSwitch()<CR>
      nmap <silent><leader><F9> :call LiteFix()<CR>

      autocmd theme BufEnter * call LiteFix()

  " Cursor ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Cursorline

      let s:contrast   = 1                  " cursorline contrast (0) low (1) high
      let s:cursorline = g:dfm_bg_light     " declare cursorline, colour is arbitrary

      function! Cursor()
        execute 'highlight Cursor guibg=' . g:dfm_cursor . ' guifg=' . g:dfm_bg
      endfunction

      " set cursorline theme
      function! HiLite()
        if ProseFT()
          execute 'highlight CursorLine gui=none guibg='   . s:cursorline . ' guifg=' . s:foreground
          execute 'highlight CursorLineNr gui=bold guibg=' . s:cursorline . ' guifg=' . s:foreground
        else
          execute 'highlight CursorLineNr gui=bold guibg=' . g:dfm_bg . ' guifg=' . g:dfm_fg
        endif
        call Cursor()
      endfunction

      function! CursorLine(fg, bg, BG)
        let s:foreground = a:fg
        if s:cursorline != a:bg
          let s:cursorline = a:bg
        else
          let s:cursorline = a:BG
        endif
      endfunction

      function! ToggleHiLite()
        if s:contrast == 0
          " call CursorLine(g:dfm_fg, g:dfm_bg, g:dfm_bg_line)
          call CursorLine(g:dfm_fg, g:dfm_bg, g:dfm_bg)
        else
          " call CursorLine(g:dfm_fg, g:dfm_bg_line, g:dfm_bg)
          call CursorLine(g:dfm_fg, g:dfm_bg, g:dfm_bg)
        endif
        call HiLite()
      endfunction

  " Font ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch font

      " let s:source_font = 'Input\ Mono\ Compressed\'
      " let s:source_font = 'PragmataPro\'
      let s:source_font   = 'Iosevka\'
      let s:prose_font    = 'Courier\ Prime\'

      function! Fontspace(prose, source)
        if argv(0) == 'vimwiki' || expand('%:e') =~ '\(wiki\|eml\|draft\)$'
          execute 'set guifont=' . s:prose_font  . ' ' . a:prose
          execute 'set linespace=' . a:prose
        else
          execute 'set guifont=' . s:source_font . ' ' . a:prose
          execute 'set linespace=' . a:source
        endif
      endfunction

      " adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
      function! FontSize(size)
        if system("lspci") =~ 'VGA .*\[GeForce GTX 970\]'
          " for desktop nvidia gpu
          if &guifont =~ '11' || a:size < 0
            call Fontspace(10, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call Fontspace(11, 0)
            " let g:lite_dfm_left_offset = 18
          endif
        elseif system("lspci") =~ 'VGA .* NVIDIA'
          " for macbook nvidia gpu
          if &guifont =~ '10' || a:size < 0
            call Fontspace(9, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call Fontspace(10, 0)
            " let g:lite_dfm_left_offset = 18
          endif
        elseif system("lspci") =~ 'VGA .* Intel'
          " for ati/intel gpu's
          if &guifont =~ '12' || a:size < 0
            call Fontspace(11, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call Fontspace(12, 0)
            " let g:lite_dfm_left_offset = 18
          endif
        else
          " for raspberry pi arm
          " call Fontspace(9, 0)
          if &guifont =~ '9' || a:size < 0
            call Fontspace(8, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call Fontspace(9, 0)
            " let g:lite_dfm_left_offset = 18
          endif
        endif
      endfunction

      function! FontSwitch()
        call FontSize(0)
        " toggle fullscreen to reposition statusline (2x required to restore)
        call system('wmctrl -ir ' . v:windowid . ' -b toggle,fullscreen')
        call system('wmctrl -ir ' . v:windowid . ' -b toggle,fullscreen')
        if ! ProseFT()
          call Quietly('LiteDFMClose')
          call LiteType()
        endif
      endfunction

      nmap <silent><C-F9> :call FontSwitch()<CR>

    " ............................................ Initial font and line spacing

      if argc() == 1
        " a bit of regex weirdness requiring *eml(?)
        call FontSize(argv(0) =~ '.*\(wiki\|draft\|*eml\|md\)$' ? +1 : -1)
      else
        call FontSize(-1)
      endif

" themes.vim
