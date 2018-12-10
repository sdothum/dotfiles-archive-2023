" sdothum - 2016 (c) wtfpl

" User Interface
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let s:cursorline           = 0          " visible (0) off (1) on
      let s:sync                 = 0          " sync (0) off (1) indent guides

    " ......................................................... DFM colour masks

      " foreground
      let s:dfm_fg_light         = g:light_fg " light foreground (high contrast)
      let s:dfm_fg_dark          = g:dark_fg  " dark foreground (high contrast)
      let s:dfm_fg_text_light    = g:mono_2   " light normal text
      let s:dfm_fg_text_dark     = g:gray5    " dark normal text
      let s:dfm_proof_light      = g:light    " light hypertext
      let s:dfm_proof_dark       = g:dark     " dark hypertext
      let s:dfm_bg_spell_light   = g:spell    " light spelling
      let s:dfm_bg_spell_dark    = g:hue_5    " dark spelling
      let s:dfm_ale_light        = g:hue_1
      let s:dfm_ale_dark         = g:hue_4

      " background
      let s:dfm_bg_light         = g:base7    " flatwhite light background
      let s:dfm_bg_dark          = g:gray1    " quantum dark background
      let s:dfm_match_light      = g:hue_5    " flatwhite light parens
      let s:dfm_match_dark       = g:red      " quantum dark parens
      let s:dfm_vsplit_light     = g:base4    " invisible split
      let s:dfm_vsplit_dark      = g:gray1    " invisible split
      let s:dfm_folded_light     = g:gray5    " vimdiff fold
      let s:dfm_folded_dark      = g:gray4    " vimdiff fold

      " cursor line
      let s:dfm_cursor_light     = g:cursor   " iA Writer
      let s:dfm_cursor_dark      = g:cursor   " iA Writer
      let s:dfm_cursorline_light = g:black    " light cursorline
      let s:dfm_cursorline_dark  = g:white    " dark cursorline
      let s:dfm_bg_line_light    = g:column   " light cursorline
      let s:dfm_bg_line_dark     = g:gray2    " dark cursorline
      let s:dfm_bg_column_light  = g:orange   " light column
      let s:dfm_bg_column_dark   = g:hue_1    " dark column
      let s:dfm_fg_line_light    = g:gray4    " light line numbers
      let s:dfm_fg_line_dark     = g:gray4    " dark line numbers

      " statusline
      let s:dfm_bg_status_light  = g:base7    " light statusline
      let s:dfm_bg_status_dark   = g:gray1    " dark statusline
      let s:dfm_fg_status_light  = g:mono_1   " light statusline
      let s:dfm_fg_status_dark   = g:mono_4   " dark statusline
      let s:dfm_fg_user1_light   = g:mono_1   " light statusline
      let s:dfm_fg_user1_dark    = g:mono_4   " dark statusline
      let s:dfm_fg_user2_light   = g:hue_2    " light statusline
      let s:dfm_fg_user2_dark    = g:cyan     " dark statusline

  " Colours ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................. Palette

      function! theme#Palette()
        Trace theme#Palette()
        execute 'let s:dfm_fg          = s:dfm_fg_'                     . &background
        execute 'let s:dfm_proof       = s:dfm_proof_'                  . &background
        execute 'let s:dfm_bg          = s:dfm_bg_'                     . &background
        execute 'let s:dfm_match       = s:dfm_match_'                  . &background
        execute 'let s:dfm_folded      = s:dfm_folded_'                 . &background
        execute 'let s:dfm_vsplit      = s:dfm_vsplit_'                 . &background
        execute 'let s:dfm_cursor      = s:dfm_cursor_'                 . &background
        execute 'let s:dfm_cursorline  = s:dfm_cursorline_'             . &background
        execute 'let s:dfm_cursor_bg   = s:cursorline == 0 ? s:dfm_bg_' . &background . ' : s:dfm_bg_line_' . &background
        execute 'let s:dfm_bg_line     = s:dfm_bg_line_'                . &background
        execute 'let s:dfm_fg_line     = s:dfm_fg_line_'                . &background
        execute 'let s:dfm_bg_status   = s:dfm_bg_status_'              . &background
        execute 'let s:dfm_fg_status   = s:dfm_fg_status_'              . &background
        execute 'let s:dfm_fg_user1    = s:dfm_fg_user1_'               . &background
        execute 'let s:dfm_fg_user2    = s:dfm_fg_user2_'               . &background
        execute 'let s:dfm_linenr_ins  = s:dfm_bg_'                     . &background
      endfunction

      " return (calculated) color variable value
      function! theme#Value(varname)
        execute 'return ' . a:varname
      endfunction

    " ............................................................. Colour theme

      " margins, selection and cursor
      function! theme#Theme()
        Trace theme#Theme()
        if !has("gui_running")
          return                            " theme is only gui compliant
        endif
        let l:background = &background == 'light' ? 'dark' : 'light'
        let l:cursor     = theme#Value('s:dfm_cursor_'  . l:background)
        let l:text       = theme#Value('s:dfm_fg_text_' . &background)
        execute 'highlight ErrorMsg        guibg=' . s:dfm_bg         . ' guifg=red'
        execute 'highlight ExtraWhitespace guibg=' . l:cursor         . ' guifg=' . theme#Value('s:dfm_bg_' . l:background)
        execute 'highlight VisualCursor    guibg=' . l:cursor         . ' guifg=' . s:dfm_bg
        execute 'highlight ReplaceCursor   guibg=' . l:cursor         . ' guifg=' . s:dfm_bg
        execute 'highlight CommandCursor   guibg=' . l:cursor         . ' guifg=' . s:dfm_bg
        execute 'highlight Folded          guibg=' . s:dfm_folded     . ' guifg=' . s:dfm_bg
        execute 'highlight User1           guibg=' . s:dfm_bg         . ' guifg=' . s:dfm_fg_user1
        execute 'highlight User2           guibg=' . s:dfm_bg         . ' guifg=' . s:dfm_fg_user2
        execute 'highlight VertSplit       guibg=' . s:dfm_vsplit     . ' guifg=' . s:dfm_vsplit
        execute 'highlight ShowMarksHLl    guibg=' . s:dfm_bg
        execute 'highlight SignColumn      guibg=' . s:dfm_bg
        execute 'highlight InsertCursor    guibg=' . s:dfm_cursor     . ' guifg=' . s:dfm_bg
        execute 'highlight CursorLine      guibg=' . s:dfm_cursor_bg  . ' guifg=' . s:dfm_cursorline
        execute 'highlight Cursor          guibg=' . s:dfm_cursor     . ' guifg=' . g:black
        execute 'highlight MatchParen      guibg=' . s:dfm_match      . ' guifg=' . s:dfm_bg     . ' gui=bold'
        execute 'highlight ALEWarningSign  guifg=' . theme#Value('s:dfm_ale_'     . &background) . ' gui=bold'
        execute 'highlight ALEErrorSign    guifg=red gui=bold'
        " toggling colorcolunm toggles spell colors (not a prose workflow issue)
        execute 'highlight SpellBad        guibg=' . theme#Value('s:dfm_bg_spell_' . &background) . ' guifg=' . l:text
        highlight! link SpellCap           SpellBad
        highlight! link SpellRare          SpellBad
        highlight! link SpellLocal         SpellBad
        " add flatwhite contrast
        if &background == 'light' && g:lightscheme == 'flatwhite'
          execute 'highlight Search        guifg=' . g:white    . ' guibg=red guisp=red gui=bold'
          execute 'highlight IncSearch     guifg=' . g:light_fg . ' guibg=' . s:dfm_cursor . ' term=none cterm=none gui=none'
          execute 'highlight StatuslineNC  guifg=' . g:white
          highlight link SneakScope        Cursor
          highlight link mkdLink           htmlString
        endif
        call theme#FzfColors()
        call theme#SignifyColors()
        call theme#IndentTheme()
        call theme#Margin()
        call theme#NoTilde()
        ColumnWrap
      endfunction

      " ruler, indents
      function! theme#IndentTheme()
        Trace theme#IndentTheme()
        execute 'highlight IndentGuidesOdd  guibg=' . theme#Value('s:dfm_bg_'        . &background)
        execute 'highlight IndentGuidesEven guibg=' . theme#Value('s:dfm_bg_line_'   . &background)
        if g:ruler == 2
          execute 'highlight ColorColumn    guibg=' . theme#Value('s:dfm_bg_column_' . &background)
        else
          execute 'highlight ColorColumn    guibg=' . theme#Value('s:dfm_bg_line_'   . &background)
        endif
        if s:sync == 1                      " refresh any indent guides, see theme#LiteSwitch()
          IndentGuidesToggle
          IndentGuidesToggle
          let s:sync = 0
        endif
      endfunction

      " line numbers
      function! theme#LineNr(mode)
        Trace theme#LineNr()
        execute 'highlight CursorLineNr '   . (g:view == 0 ? 'gui=bold guifg=' . theme#Value('s:dfm_bg_' . &background)
                \                                          : 'gui=none guifg=' . (b:proof == 0 ? s:dfm_bg : s:dfm_fg_line))
        let s:dfm_linenr_cmd = g:view == 0  ? s:dfm_fg_line  : s:dfm_bg
        if a:mode == 'n'
          execute 'highlight LineNr guifg=' . s:dfm_linenr_cmd
        else
          execute 'highlight LineNr guifg=' . s:dfm_linenr_ins
        endif
        execute 'highlight NonText guifg=red'
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
          execute 'highlight SignifyLineAdd    guibg=' . s:dfm_bg . ' guifg=' . g:hue_3
          execute 'highlight SignifyLineChange guibg=' . s:dfm_bg . ' guifg=' . g:hue_2
          execute 'highlight SignifyLineDelete guibg=' . s:dfm_bg . ' guifg=' . g:hue_5_2
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

      function! theme#ColorScheme(contrast)
        if a:contrast == 0
          " must set background before colorscheme for flatwhite colors
          let &background = 'light'
          execute 'colorscheme ' . g:lightscheme
        else
          let &background = 'dark'
          colorscheme quantum
        endif
      endfunction

      " toggle colour scheme
      function! theme#LiteSwitch()
        Trace theme#LiteSwitch()
        " trap and ignore initialization error
        Quietly LiteDFMClose
        if &background == 'light'
          call theme#ColorScheme(1)
        else
          call theme#ColorScheme(0)
        endif
        let s:sync = 1                      " see theme#IndentTheme()
        call ui#LiteType()
      endfunction

  " Font ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch font

      " balance left right margins with font size changes (and window resizing)
      function! theme#Margin()
        Trace theme#Margin()
        " account for linenr <space> text
        let g:lite_dfm_left_offset = max([1, min([22, (&columns - &textwidth - 4) / 2])])
        Quietly LiteDFM
        call theme#LineNr(mode())
        call ui#RefreshInfo()
      endfunction

      function! theme#Font(size)
        Trace theme#Font()
        execute 'set guifont=' . (core#Prose() ? g:prose_font : g:source_font) . ' ' . a:size
        if exists('s:size')
          " gui redrawi for font size change
          RedrawGui
        endif
        let s:size = a:size
      endfunction

      " adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
      function! theme#FontSize(type)
        Trace theme#FontSize()
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
        Trace theme#FontSwitch()
        call theme#FontSize(g:font_type == 1 ? 0 : 1)
        if !core#Prose()
          Quietly LiteDFMClose
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
        if core#Prose()
          execute 'highlight CursorLine gui=none guibg=' . s:dfm_bg . ' guifg=' . s:dfm_fg
        endif
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
          execute 'highlight EndOfBuffer ctermfg=black guifg=' . s:dfm_bg
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
