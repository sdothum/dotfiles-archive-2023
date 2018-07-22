" sdothum - 2016 (c) wtfpl

" User Interface
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let s:cursorline          = 0         " visible (0) off (1) on
      let s:sync                = 0         " sync (0) off (1) indent guides

    " ......................................................... DFM colour masks

      " foreground
      let s:dfm_fg_light        = '#000000' " light foreground (high contrast)
      let s:dfm_fg_dark         = '#ffffff' " dark foreground (high contrast)
      let s:dfm_proof_light     = '#dddddd' " dark foreground
      let s:dfm_proof_dark      = '#444444' " light foreground
      let s:dfm_ale_light       = g:rgb_1
      let s:dfm_ale_dark        = g:red

      " background
      let s:dfm_bg_light        = g:rgb_15  " solarized light (paper) background
      let s:dfm_bg_dark         = g:gray1   " quantum dark background
      let s:dfm_match_light     = g:rgb_1   " solarized light parens
      let s:dfm_match_dark      = g:red     " quantum dark parens
      let s:dfm_vsplit_light    = g:rgb_15  " invisible split
      let s:dfm_vsplit_dark     = g:gray1   " invisible split
      let s:dfm_folded_light    = g:gray5   " vimdiff fold
      let s:dfm_folded_dark     = g:gray4   " vimdiff fold

      " cursor line
      let s:dfm_cursor_light    = '#20bbfc' " iA Writer
      let s:dfm_cursor_dark     = '#20bbfc' " iA Writer
      let s:dfm_bg_line_light   = g:rgb_7   " light cursorline
      let s:dfm_bg_line_dark    = g:gray2   " dark cursorline
      let s:dfm_bg_column_light = '#E2D7B6' " light column (darker shade of rgb_7)
      let s:dfm_bg_column_dark  = '#0A4C5C' " dark column (lighter shade of rgb_0)
      let s:dfm_fg_line_light   = g:rgb_14  " light line numbers
      let s:dfm_fg_line_dark    = g:gray4   " dark line numbers

      " statusline
      let s:dfm_bg_status_light = g:rgb_15  " light statusline
      let s:dfm_bg_status_dark  = g:gray1   " dark statusline
      let s:dfm_fg_status_light = g:rgb_0   " light statusline
      let s:dfm_fg_status_dark  = g:rgb_7   " dark statusline
      let s:dfm_fg_user1_light  = g:rgb_0   " light statusline
      let s:dfm_fg_user1_dark   = g:rgb_7   " dark statusline
      let s:dfm_fg_user2_light  = g:rgb_4   " light statusline
      let s:dfm_fg_user2_dark   = g:cyan    " dark statusline

  " Colours ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................. Palette

      function! theme#Palette()
        call core#Trace('theme#Palette()')
        execute 'let s:dfm_fg          = s:dfm_fg_'                     . &background
        execute 'let s:dfm_proof       = s:dfm_proof_'                  . &background
        execute 'let s:dfm_bg          = s:dfm_bg_'                     . &background
        execute 'let s:dfm_match       = s:dfm_match_'                  . &background
        execute 'let s:dfm_folded      = s:dfm_folded_'                 . &background
        execute 'let s:dfm_vsplit      = s:dfm_vsplit_'                 . &background
        execute 'let s:dfm_cursor      = s:dfm_cursor_'                 . &background
        execute 'let s:dfm_bg_line     = s:dfm_bg_line_'                . &background
        execute 'let s:dfm_fg_line     = s:dfm_fg_line_'                . &background
        execute 'let s:dfm_bg_status   = s:dfm_bg_status_'              . &background
        execute 'let s:dfm_fg_status   = s:dfm_fg_status_'              . &background
        execute 'let s:dfm_fg_user1    = s:dfm_fg_user1_'               . &background
        execute 'let s:dfm_fg_user2    = s:dfm_fg_user2_'               . &background
        execute 'let s:dfm_cursorline  = s:cursorline == 0 ? s:dfm_bg_' . &background . ' : s:dfm_bg_line_' . &background
        execute 'let s:dfm_linenr_ins  = s:dfm_bg_'                     . &background
      endfunction

      " return (calculated) color variable value
      function! theme#Value(varname)
        execute 'return ' . a:varname
      endfunction

    " ............................................................. Colour theme

      " margins, selection and cursor
      function! theme#Theme()
        call core#Trace('theme#Theme()')
        let l:background = &background == 'light' ? 'dark' : 'light'
        execute 'highlight ExtraWhitespace guibg=' . theme#Value('s:dfm_cursor_'    . l:background) . ' guifg=' . theme#Value('s:dfm_bg_' . l:background)
        execute 'highlight VisualCursor    guibg=' . theme#Value('s:dfm_cursor_'    . l:background) . ' guifg=' . s:dfm_bg
        execute 'highlight ReplaceCursor   guibg=' . theme#Value('s:dfm_cursor_'    . l:background) . ' guifg=' . s:dfm_bg
        execute 'highlight CommandCursor   guibg=' . theme#Value('s:dfm_cursor_'    . l:background) . ' guifg=' . s:dfm_bg
        execute 'highlight Folded          guibg=' . s:dfm_folded                                   . ' guifg=' . s:dfm_bg
        execute 'highlight User1           guibg=' . s:dfm_bg                                       . ' guifg=' . s:dfm_fg_user1
        execute 'highlight User2           guibg=' . s:dfm_bg                                       . ' guifg=' . s:dfm_fg_user2
        execute 'highlight VertSplit       guibg=' . s:dfm_vsplit                                   . ' guifg=' . s:dfm_vsplit
        execute 'highlight ShowMarksHLl    guibg=' . s:dfm_bg
        execute 'highlight SignColumn      guibg=' . s:dfm_bg
        execute 'highlight InsertCursor    guibg=' . s:dfm_cursor                                   . ' guifg=' . s:dfm_bg
        execute 'highlight CursorLine      guibg=' . s:dfm_cursorline                               . ' gui=none'
        execute 'highlight Cursor          guibg=' . s:dfm_cursor                                   . ' guifg=' . s:dfm_bg
        execute 'highlight MatchParen      guibg=' . s:dfm_match                                    . ' guifg=' . s:dfm_bg . ' gui=bold'
        execute 'highlight ALEErrorSign    guifg=' . theme#Value('s:dfm_ale_'    . l:background)
        execute 'highlight ALEWarningSign  guifg=' . s:dfm_fg
        call theme#FzfColors()
        call theme#SignifyColors()
        call theme#IndentTheme()
        call theme#Margin()
        call theme#NoTilde()
      endfunction

      " ruler, indents
      function! theme#IndentTheme()
        call core#Trace('theme#IndentTheme()')
        execute 'highlight IndentGuidesOdd     guibg=' . theme#Value('s:dfm_bg_'        . &background)
        execute 'highlight IndentGuidesEven    guibg=' . theme#Value('s:dfm_bg_line_'   . &background)
        if g:ruler == 2
          execute 'highlight ColorColumn       guibg=' . theme#Value('s:dfm_bg_column_' . &background)
        else
          execute 'highlight ColorColumn       guibg=' . theme#Value('s:dfm_bg_line_'   . &background)
        endif
        if s:sync == 1                      " refresh any indent guides, see theme#LiteSwitch()
          execute 'IndentGuidesToggle'
          execute 'IndentGuidesToggle'
          let s:sync = 0
        endif
      endfunction

      " line numbers
      function! theme#LineNr(mode)
        call core#Trace('theme#LineNr()')
        execute 'highlight CursorLineNr ' . (g:view == 0 ? 'gui=bold guifg=' . theme#Value('s:dfm_fg_' . &background)
                \                                        : 'gui=none guifg=' . (b:proof == 0 ? s:dfm_bg : s:dfm_fg_line))
        let s:dfm_linenr_cmd = g:view == 0  ? s:dfm_fg_line  : s:dfm_bg
        if a:mode == 'n'
          execute 'highlight LineNr guifg=' . s:dfm_linenr_cmd
        else
          execute 'highlight LineNr guifg=' . s:dfm_linenr_ins
        endif
      endfunction

      " g:fzf_colors initializes fzf only once, so override cursorline color
      function! theme#FzfColors()
        " cannot appear to set other colors, such as hl+ (?)
        let $FZF_DEFAULT_OPTS = '--reverse --color=fg+:' . theme#Value('s:dfm_fg_' . &background)

        " hide bottom fzf window identifier
        execute 'highlight fzf1 guibg=' . s:dfm_bg . ' guifg=' . s:dfm_bg
        execute 'highlight fzf2 guibg=' . s:dfm_bg . ' guifg=' . s:dfm_bg
        execute 'highlight fzf3 guibg=' . s:dfm_bg . ' guifg=' . s:dfm_bg
      endfunction

      function! theme#SignifyColors()
        if &background == 'light'
          highlight link SignifyLineAdd    Statement
          highlight link SignifyLineChange Type
          highlight link SignifyLineDelete Special
        else
          highlight link SignifyLineAdd    String
          highlight link SignifyLineChange Type
          highlight link SignifyLineDelete Identifier
        endif
        highlight link SignifySignAdd      SignifyLineAdd
        highlight link SignifySignChange   SignifyLineChange
        highlight link SignifySignDelete   SignifyLineDelete
      endfunction

  " Theme ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Switch colour

      " toggle colour scheme
      function! theme#LiteSwitch()
        call core#Trace('theme#LiteSwitch()')
        " trap and ignore initialization error
        call core#Quietly('LiteDFMClose')
        if &background == 'light'
          let &background = 'dark'
          colorscheme quantum
        else
          let &background = 'light'
          colorscheme solarized8_high
        endif
        let s:sync = 1                      " see theme#IndentTheme()
        call ui#LiteType()
      endfunction

  " Font ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch font

      " balance left right margins with font size changes (and window resizing)
      function! theme#Margin()
        call core#Trace('theme#Margin()')
        " account for linenr <space> text
        let g:lite_dfm_left_offset = max([1, min([22, (&columns - &textwidth - 4) / 2])])
        call core#Quietly('LiteDFM')
        call theme#LineNr(mode())
        call ui#RefreshInfo()
      endfunction

      function! theme#Font(size)
        call core#Trace('theme#Font()')
        execute 'set guifont=' . (core#Prose() ? g:prose_font : g:source_font) . ' ' . a:size
        if exists('s:size')
          call core#RedrawGui()             " gui redrawi for font size change
        endif
        let s:size = a:size
      endfunction

      " adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
      function! theme#FontSize(type)
        call core#Trace('theme#FontSize()')
        if $DISPLAY > ''
          if g:font_type != a:type
            let g:font_type = a:type
            let l:size      = system('fontsize')
            call theme#Font(a:type == 0 ? l:size : l:size + g:font_step)
            set laststatus=2                " turn on statusline
          endif
        endif
      endfunction

      function! theme#FontSwitch()
        call core#Trace('theme#FontSwitch()')
        call theme#FontSize(g:font_type == 1 ? 0 : 1)
        if !core#Prose()
          call core#Quietly('LiteDFMClose')
          call ui#LiteType()
        endif
      endfunction

  "  Distraction free highlight ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Code view

      function! theme#CodeView()
        execute 'highlight LineNr guifg=' . s:dfm_fg_line
      endfunction

    " .................................................... Distraction free view

      " prose style
      function! theme#DfmView()
        execute 'highlight CursorLine gui=none guibg=' . s:dfm_bg . ' guifg=' . s:dfm_fg
      endfunction

    " .............................................................. Switch View

      " toggle full document highlight
      function! theme#ToggleProof()
        execute 'let g:limelight_conceal_guifg="' . s:dfm_proof . '"'
      endfunction

    " .............................................................. EOF markers

      function! theme#NoTilde()
        " hide tilde marker (not applicable to console)
        if $DISPLAY > ''
          execute 'highlight EndOfBuffer guifg=' . s:dfm_bg
          " reset menu highlight after loading autocompletion plugin
          execute 'highlight PmenuSel term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=' . s:dfm_bg_line . ' guibg=' . s:dfm_bg
          " match command line tab menu
          execute 'highlight WildMenu term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=' . s:dfm_bg_line . ' guibg=' . s:dfm_bg
        endif
      endfunction

  " Context statusline highlight ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Statusline

      function! theme#ShowStatusline()
        " undo statusline gui=reverse
        execute 'highlight Statusline gui=none guibg=' . s:dfm_bg_status . ' guifg=' . s:dfm_fg_status
        set laststatus=2
      endfunction

      function! theme#ShowInfo()
        execute 'highlight Statusline guibg=' . s:dfm_bg
      endfunction

" theme.vim
