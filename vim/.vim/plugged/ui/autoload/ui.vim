" sdothum - 2016 (c) wtfpl

" User Interface
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Theme ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let s:cursorline    = 0               " visible (0) off (1) on
      let s:view          = 0               " mixed view (filetype) handling
      let s:wikiinfo      = 1               " initial wikiinfo

      " let s:source_font = 'Input\ Mono\ Compressed\'
      " let s:source_font = 'PragmataPro\'
      let s:source_font   = 'Iosevka\'
      " let s:prose_font  = 'Courier\ Prime\'
      let s:prose_font    = 'Iosevka\'

    " .................................................................. Palette

      " solarized colour palette (see vim-solarized8)
      let s:rgb_0    = '#073642'            " base02 dark highlight
      " let s:rgb_1  = '#dc322f'            " red
      " let s:rgb_2  = '#719e07'            " green
      let s:rgb_3    = '#b58900'            " yellow
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
      let g:dfm_cursor_light    = '#52A5E0' " light cursor (lighter shade of rgb_4)
      let g:dfm_cursor_dark     = '#E56865' " dark cursor (lighter shade of rgb_1)
      let g:dfm_bg_line_light   = s:rgb_7   " light cursorline
      let g:dfm_bg_line_dark    = s:rgb_0   " dark cursorline
      let g:dfm_bg_column_light = '#E2D7B6' " light column (darker shade of rgb_7)
      let g:dfm_bg_column_dark  = '#0A4C5C' " dark column (lighter shade of rgb_0)
      let g:dfm_fg_line_light   = '#cccccc' " light line numbers
      let g:dfm_fg_line_dark    = '#555555' " dark line numbers

      " statusline
      let g:dfm_status_light    = s:rgb_0   " light statusline
      let g:dfm_status_dark     = s:rgb_7   " dark statusline

    " .................................................................. Palette

      function! ui#Palette()
        if &background == 'light'
          let g:dfm_fg          = g:dfm_fg_light
          let g:dfm_proof       = g:dfm_proof_light
          let g:dfm_unfocused   = g:dfm_unfocused_light
          let g:dfm_bg          = g:dfm_bg_light
          let g:dfm_cursor      = g:dfm_cursor_light
          let g:dfm_bg_line     = g:dfm_bg_line_light
          let g:dfm_fg_line     = g:dfm_fg_line_light
          let g:dfm_status      = g:dfm_status_light
          let g:dfm_cursorline  = s:cursorline == 0 ? g:dfm_bg_light : g:dfm_bg_line_light
          let g:dfm_linenr_ins  = g:dfm_bg_light
          let g:dfm_linenr_cmd  = Prose() ? g:dfm_bg_light : g:dfm_fg_line_light
        else
          let g:dfm_fg          = g:dfm_fg_dark
          let g:dfm_proof       = g:dfm_proof_dark
          let g:dfm_unfocused   = g:dfm_unfocused_dark
          let g:dfm_bg          = g:dfm_bg_dark
          let g:dfm_cursor      = g:dfm_cursor_dark
          let g:dfm_bg_line     = g:dfm_bg_line_dark
          let g:dfm_fg_line     = g:dfm_fg_line_dark
          let g:dfm_status      = g:dfm_status_dark
          let g:dfm_cursorline  = s:cursorline == 0 ? g:dfm_bg_dark : g:dfm_bg_line_dark
          let g:dfm_linenr_ins  = g:dfm_bg_dark
          let g:dfm_linenr_cmd  = Prose() ? g:dfm_bg_dark : g:dfm_fg_line_dark
        endif
      endfunction

    " ................................................................... Screen

      " margins, selection and cursor
      function! ui#Theme()
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
        execute 'highlight CursorLine gui=none   guibg=' . g:dfm_cursorline
        execute 'highlight ALEErrorSign          guifg=' . g:dfm_fg
        execute 'highlight ALEWarningSign        guifg=' . s:rgb_3
        call ui#Cursor()
      endfunction

      function! ui#Cursor()
        execute 'highlight Cursor gui=bold guibg=' . g:dfm_cursor . ' guifg=' . g:dfm_bg
      endfunction

    " Contrast ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Switch colour

      " toggle colour scheme
      function! ui#LiteSwitch()
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
        call ui#LiteType()
      endfunction

      function! ui#LiteFix()
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

  " Font ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch font

      function! ui#Fontspace(prose, source)
        if Prose()
          execute 'set guifont='   . s:prose_font  . ' ' . a:prose
          execute 'set linespace=' . a:prose
        else
          execute 'set guifont='   . s:source_font . ' ' . a:prose
          execute 'set linespace=' . a:source
        endif
      endfunction

      function! ui#FontSwitch()
        call FontSize(g:size == -1 ? +1 : -1)
        if ! Prose()
          call Quietly('LiteDFMClose')
          call ui#LiteType()
        endif
      endfunction

  "  Distraction free modes ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Code view

      " source code style
      function! ui#CodeView()
        " restore CursorLine syntax highlighting (if altered by ProseView)
        if s:view != 0
          syntax enable
        endif
        let s:view = 0
        execute 'Limelight!'
        call ui#Theme()
        call IndentTheme()
        call ui#LiteFix()
        execute 'highlight LineNr guifg=' . g:dfm_fg_line
        call lightline#colorscheme()
        set laststatus=2                    " turn on statusline
        set showmode
      endfunction

    " ............................................................... Prose view

      " vimwiki prose style
      function! ui#ProseView()
        let s:view = 1
        " silent !tmux set status off
        " set numberwidth=1                 " goyo settings
        " set nonumber
        " set fillchars-=stl:.              " remove statusline fillchars '.' set by goyo.vim
        " set fillchars+=stl:\ "
        call ui#Theme()
        call ui#DfmWriting()
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
      function! ui#DfmWriting()
        execute 'highlight Normal guifg=' . g:dfm_unfocused
        let s:proof = 0
        call ui#ShowInfo(0)
      endfunction

      " toggle full document highlight
      function! ui#ToggleProof()
        call ui#Theme()
        if s:proof == 0
          if &filetype == 'vimwiki'
            " hack to apply more robust markdown syntax highlighting
            " note: this hack may break vimwiki navigation in the future!
            set filetype=markdown
            call ui#ProseView()
            " force margin centering
            call Refresh()
          endif
          execute 'highlight Normal guifg=' . g:dfm_proof
          let s:proof = 1
          call ui#ShowInfo(1)
          execute 'Limelight!'
        else
          call ui#DfmWriting()
          execute 'Limelight'
        endif
      endfunction

  " Screen focus ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Screen display

      function! ui#LiteType()
        call ui#Palette()
        if Prose()
          call ui#ProseView()
        else
          call ui#CodeView()
        endif
      endfunction

  " Enhanced statusline ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Statusline format

      " center dfm indicator / proofing statusline
      function! ui#WikiInfo(proof)
        try                                 " trap snippet insertion interruption
          let g:prose  = 1
          if a:proof == 0
            let l:name = (&modified ? '' : '')
          else
            let l:name = expand('%:t:r') . (&modified ? '    ' : ' ⎵ ')  . WordCount()
          endif
          let l:leader = repeat(' ', (winwidth(0) - strlen(l:name)) / 2 + 2)
          return l:leader . l:name
        catch
        endtry
      endfunction

      function! ui#ShowInfo(proof)
        if s:wikiinfo == 1
          " set statusline=%=%{expand('%:t:r')}\ \\ %{WordCount()}%{(&modified\ ?\ '\ +'\ :\ '')}
          execute 'set statusline=%{ui#WikiInfo(' . a:proof . ')}'
          " goyo defines highlight term/gui reverse
          execute 'highlight statusline guibg=' . g:dfm_status . ' guifg=' . g:dfm_bg
          set laststatus=2
        else
          " simply hide statusline content
          execute 'highlight statusline guibg=' . g:dfm_bg
        endif
      endfunction

    " ........................................................ Toggle statusline

      function! ui#ToggleInfo()
        if Prose()                          " toggle between writing and proofing modes
          call ui#ToggleProof()
        else
          call ui#CodeView()                   " refresh margin
          let g:code = (g:code == 0 ? 1 : 0)
        endif
        call Refresh()                      " if margins not properly set, do it now
        call ui#Cursor()                       " restore cursor (fullscreen toggling reverts defaults)
      endfunction

" views.vim
