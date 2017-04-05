" sdothum - 2016 (c) wtfpl

" Themes
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      " solarized colour pallette (see vim-solarized8)
      let s:rgb_0    = '#073642'            " base02 dark highlight
      " let s:rgb_1  = '#dc322f'            " red
      " let s:rgb_2  = '#719e07'            " green
      " let s:rgb_3  = '#b58900'            " yellow
      let s:rgb_4    = '#268bd2'            " blue
      " let s:rgb_5  = '#d33682'            " magenta
      " let s:rgb_6  = '#2aa198'            " cyan
      let s:rgb_7    = '#eee8d5'            " base2 light highlight
      let s:rgb_8    = '#002b36'            " base03 dark bg
      " let s:rgb_9  = '#cb4b16'            " orange
      let s:rgb_10   = '#586e75'            " base01 darkest grey
      let s:rgb_11   = '#657b83'            " base00 dark grey
      let s:rgb_12   = '#839496'            " base0 light grey
      " let s:rgb_13 = '#6c71c4'            " violet
      let s:rgb_14   = '#93a1a1'            " base1 lightest grey
      let s:rgb_15   = '#fdf6e3'            " base3 light bg

      augroup theme
        autocmd!
      augroup END

    " ......................................................... DFM colour masks

      " foreground
      let g:dfm_fg_light        = 'brown'   " light foreground
      let g:dfm_fg_dark         = 'yellow'  " dark foreground
      let g:dfm_proof_light     = s:rgb_8   " dark foreground
      let g:dfm_proof_dark      = s:rgb_15  " light foreground
      let g:dfm_unfocused_light = s:rgb_8   " light grey surrounding text content
      let g:dfm_unfocused_dark  = s:rgb_15  " dark grey surrounding text content

      " background
      let g:dfm_bg_light        = s:rgb_15  " solarized light (paper) background
      let g:dfm_bg_dark         = s:rgb_8   " solarized dark background

      " cursor line
      let g:dfm_cursor_light    = '#00ACE4' " light bluish cursor
      let g:dfm_cursor_dark     = '#EC7146' " dark reddish cursor
      let g:dfm_bg_line_light   = s:rgb_7   " light cursorline
      let g:dfm_bg_line_dark    = s:rgb_0   " dark cursorline
      let g:dfm_bg_column_light = '#E2D7B6' " light column
      let g:dfm_bg_column_dark  = '#0A4C5C' " dark column
      let g:dfm_fg_line_light   = '#cccccc' " light line numbers
      let g:dfm_fg_line_dark    = '#555555' " dark line numbers

      " statusline
      let g:dfm_status_light    = s:rgb_0   " light statusline
      let g:dfm_status_dark     = s:rgb_7   " dark statusline

    " .................................................................. Palette

      function! Palette()
        if &background == 'light'
          let g:dfm_fg          = g:dfm_fg_light
          let g:dfm_proof       = g:dfm_proof_light
          let g:dfm_unfocused   = g:dfm_unfocused_light
          let g:dfm_bg          = g:dfm_bg_light
          let g:dfm_cursor      = g:dfm_cursor_light
          let g:dfm_bg_line     = g:dfm_bg_line_light
          let g:dfm_fg_line     = g:dfm_fg_line_light
          let g:dfm_status      = g:dfm_status_light
        else
          let g:dfm_fg          = g:dfm_fg_dark
          let g:dfm_proof       = g:dfm_proof_dark
          let g:dfm_unfocused   = g:dfm_unfocused_dark
          let g:dfm_bg          = g:dfm_bg_dark
          let g:dfm_cursor      = g:dfm_cursor_dark
          let g:dfm_bg_line     = g:dfm_bg_line_dark
          let g:dfm_fg_line     = g:dfm_fg_line_dark
          let g:dfm_status      = g:dfm_status_dark
        endif
      endfunction

    " ................................................................... Screen

      " margins, selection and cursor
      function! Theme()
        let g:lite_dfm_left_offset = max([ 1, min([ 22, (&columns - &textwidth) / 2 ]) ])
        call Quietly('LiteDFM')
        execute 'highlight ShowMarksHLl          guibg=' . g:dfm_bg
        execute 'highlight SignColumn            guibg=' . g:dfm_bg
        execute 'highlight CursorLineNr gui=bold guibg=' . g:dfm_bg           . ' guifg=' . g:dfm_fg
        execute 'highlight InsertCursor          guibg=' . g:dfm_cursor       . ' guifg=' . g:dfm_bg
        if &background == 'light'
          execute 'highlight ExtraWhitespace     guibg=' . g:dfm_cursor_dark  . ' guifg=' . g:dfm_bg_dark
          execute 'highlight VisualCursor        guibg=' . g:dfm_cursor_dark  . ' guifg=' . g:dfm_bg
          execute 'highlight ReplaceCursor       guibg=' . g:dfm_cursor_dark  . ' guifg=' . g:dfm_bg
          execute 'highlight CommandCursor       guibg=' . g:dfm_cursor_dark  . ' guifg=' . g:dfm_bg
          execute 'highlight IndentGuidesOdd     guibg=' . g:dfm_bg_light
          execute 'highlight IndentGuidesEven    guibg=' . g:dfm_bg_line_dark
        else
          execute 'highlight ExtraWhitespace     guibg=' . g:dfm_cursor_light . ' guifg=' . g:dfm_bg_light
          execute 'highlight VisualCursor        guibg=' . g:dfm_cursor_light . ' guifg=' . g:dfm_bg
          execute 'highlight ReplaceCursor       guibg=' . g:dfm_cursor_light . ' guifg=' . g:dfm_bg
          execute 'highlight CommandCursor       guibg=' . g:dfm_cursor_light . ' guifg=' . g:dfm_bg
          execute 'highlight IndentGuidesOdd     guibg=' . g:dfm_bg_dark
          execute 'highlight IndentGuidesEven    guibg=' . g:dfm_bg_line_dark
        endif
        call Cursor()
      endfunction

      function! Cursor()
        execute 'highlight Cursor gui=bold guibg=' . g:dfm_cursor . ' guifg=' . g:dfm_bg
      endfunction

    " Contrast ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Switch colour

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
        if ! Prose()
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
        call LiteType()
      endfunction

      nmap <silent><F7> :call LiteSwitch()<CR>

      function! LiteFix()
        " fix solarized colour shifts resulting from toggling(!)
        if &background == 'light'
          execute 'highlight LightlineLeft_normal_0    ctermfg=230 ctermbg=33  guifg=' . s:rgb_15 ' guibg=' . s:rgb_4
          execute 'highlight LightlineLeft_normal_0_1  ctermfg=33  ctermbg=244 guifg=' . s:rgb_4  ' guibg=' . s:rgb_12
          execute 'highlight LightlineLeft_normal_1    ctermfg=230 ctermbg=244 guifg=' . s:rgb_15 ' guibg=' . s:rgb_12
          execute 'highlight LightlineLeft_normal_1_2  ctermfg=244 ctermbg=187 guifg=' . s:rgb_12 ' guibg=' . s:rgb_7
          execute 'highlight LightlineRight_normal_0   ctermfg=230 ctermbg=239 guifg=' . s:rgb_15 ' guibg=' . s:rgb_10
          execute 'highlight LightlineRight_normal_0_1 ctermfg=239 ctermbg=244 guifg=' . s:rgb_10 ' guibg=' . s:rgb_12
          execute 'highlight LightlineRight_normal_1   ctermfg=230 ctermbg=244 guifg=' . s:rgb_15 ' guibg=' . s:rgb_12
          execute 'highlight LightlineRight_normal_1_2 ctermfg=244 ctermbg=187 guifg=' . s:rgb_12 ' guibg=' . s:rgb_7
        else
          execute 'highlight LightlineLeft_normal_0    ctermfg=234 ctermbg=33  guifg=' . s:rgb_8  ' guibg=' . s:rgb_4
          execute 'highlight LightlineLeft_normal_0_1  ctermfg=33  ctermbg=240 guifg=' . s:rgb_4  ' guibg=' . s:rgb_11
          execute 'highlight LightlineLeft_normal_1    ctermfg=234 ctermbg=240 guifg=' . s:rgb_8  ' guibg=' . s:rgb_11
          execute 'highlight LightlineLeft_normal_1_2  ctermfg=240 ctermbg=235 guifg=' . s:rgb_11 ' guibg=' . s:rgb_0
          execute 'highlight LightlineRight_normal_0   ctermfg=234 ctermbg=245 guifg=' . s:rgb_8  ' guibg=' . s:rgb_14
          execute 'highlight LightlineRight_normal_0_1 ctermfg=245 ctermbg=240 guifg=' . s:rgb_14 ' guibg=' . s:rgb_11
          execute 'highlight LightlineRight_normal_1   ctermfg=234 ctermbg=240 guifg=' . s:rgb_8  ' guibg=' . s:rgb_11
          execute 'highlight LightlineRight_normal_1_2 ctermfg=240 ctermbg=235 guifg=' . s:rgb_11 ' guibg=' . s:rgb_0
        endif
      endfunction

      nmap <silent><C-F7>      :call LiteFix()<CR>

      autocmd theme BufEnter * call LiteFix()

  " Font ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch font

      let s:size          = 0
      " let s:source_font = 'Input\ Mono\ Compressed\'
      " let s:source_font = 'PragmataPro\'
      let s:source_font   = 'Iosevka\'
      " let s:prose_font  = 'Courier\ Prime\'
      let s:prose_font    = 'Iosevka\'

      function! Fontspace(prose, source)
        if Prose()
          execute 'set guifont='   . s:prose_font  . ' ' . a:prose
          execute 'set linespace=' . a:prose
        else
          execute 'set guifont='   . s:source_font . ' ' . a:prose
          execute 'set linespace=' . a:source
        endif
      endfunction

      " adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
      function! FontSize(size)
        if s:size == a:size
          return
        endif
        let s:size = a:size

        if system("lspci") =~ 'VGA .*\[GeForce GTX 970\]'
          " for desktop nvidia gpu
          if &guifont =~ '11' || a:size < 0
            call Fontspace(10, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call Fontspace(11, 1)
            " let g:lite_dfm_left_offset = 18
          endif
        elseif system("lspci") =~ 'VGA .* NVIDIA'
          " for macbook nvidia gpu
          if &guifont =~ '10' || a:size < 0
            call Fontspace(9, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call Fontspace(10, 1)
            " let g:lite_dfm_left_offset = 18
          endif
        elseif system("lspci") =~ 'VGA .* Intel'
          " for ati/intel gpu's
          if &guifont =~ '12' || a:size < 0
            call Fontspace(11, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call Fontspace(12, 1)
            " let g:lite_dfm_left_offset = 18
          endif
        else
          " for raspberry pi arm
          " call Fontspace(9, 0)
          if &guifont =~ '9' || a:size < 0
            call Fontspace(8, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call Fontspace(9, 1)
            " let g:lite_dfm_left_offset = 18
          endif
        endif

        " fix statusline/commandline position (drawn outside window)
        sleep 10m                           " delay long enough for font refresh
        call ToggleGui()
        call ToggleGui()
      endfunction

      autocmd theme BufEnter * call FontSize(Prose() ? +1 : -1)

      function! FontSwitch()
        call FontSize(s:size == -1 ? +1 : -1)
        if ! Prose()
          call Quietly('LiteDFMClose')
          call LiteType()
        endif
      endfunction

      nmap <silent><S-F7>      :call FontSwitch()<CR>

" themes.vim
