" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

  " The look ___________________________________________________________________

    " .................................................................... Setup

      let s:cursorline           = 0  " visible (0) off (1) on
      let s:sync                 = 0  " sync (0) off (1) indent guides

    " ......................................................... DFM colour masks

      " foreground
      let s:dfm_fg_light         = g:light_fg   " light foreground (high contrast)
      let s:dfm_fg_dark          = g:dark_fg    " dark foreground (high contrast)
      let s:dfm_ale_dark         = g:hue_4
      let s:dfm_ale_light        = g:hue_1
      let s:dfm_bg_spell_dark    = g:hue_5      " dark spelling
      let s:dfm_bg_spell_light   = g:spell      " light spelling
      let s:dfm_fg_spell_dark    = g:base6      " dark spelling
      let s:dfm_fg_spell_light   = g:mono_2     " light spelling
      let s:dfm_proof_dark       = g:dark       " dark hypertext
      let s:dfm_proof_light      = g:light      " light hypertext

      " background
      let s:dfm_bg_light         = g:base7      " flatwhite light background
      let s:dfm_bg_dark          = g:gray1      " quantum dark background
      let s:dfm_folded_dark      = g:gray4      " vimdiff fold
      let s:dfm_folded_light     = g:gray5      " vimdiff fold
      let s:dfm_match_dark       = g:red        " quantum dark parens
      let s:dfm_match_light      = g:hue_5      " flatwhite light parens
      let s:dfm_vsplit_dark      = g:gray1      " invisible split
      let s:dfm_vsplit_light     = g:base4      " invisible split

      " cursor line
      let s:dfm_cursor_light     = g:cursor     " iA Writer
      let s:dfm_cursor_dark      = g:cursor     " iA Writer
      let s:dfm_cursorline_light = g:blue       " light cursorline
      let s:dfm_cursorline_dark  = g:white      " dark cursorline
      let s:dfm_bg_column_dark   = g:hue_1      " dark column
      let s:dfm_bg_column_light  = g:orange_bg  " light column
      let s:dfm_bg_line_dark     = g:gray2      " dark cursorline
      let s:dfm_bg_line_light    = g:blue_bg    " light cursorline
      let s:dfm_fg_line_dark     = g:gray4      " dark line numbers
      let s:dfm_fg_line_light    = g:hue_2      " light line numbers

      " statusline
      let s:dfm_bg_status_light  = g:base7      " light statusline
      let s:dfm_bg_status_dark   = g:gray1      " dark statusline
      let s:dfm_fg_status_light  = g:mono_1     " light statusline
      let s:dfm_fg_status_dark   = g:mono_4     " dark statusline
      let s:dfm_fg_user1_light   = g:mono_1     " light statusline
      let s:dfm_fg_user1_dark    = g:mono_4     " dark statusline
      let s:dfm_fg_user2_light   = g:hue_2      " light statusline
      let s:dfm_fg_user2_dark    = g:cyan       " dark statusline

  " Colours ____________________________________________________________________

    " .................................................................. Palette

      function! theme#Palette()
        Trace theme#Palette()
        execute 'let s:dfm_bg_line     = s:dfm_bg_line_'                . &background
        execute 'let s:dfm_bg          = s:dfm_bg_'                     . &background
        execute 'let s:dfm_bg_status   = s:dfm_bg_status_'              . &background
        execute 'let s:dfm_cursor_bg   = s:cursorline == 0 ? s:dfm_bg_' . &background . ' : s:dfm_bg_line_' . &background
        execute 'let s:dfm_cursorline  = s:dfm_cursorline_'             . &background
        execute 'let s:dfm_cursor      = s:dfm_cursor_'                 . &background
        execute 'let s:dfm_fg_line     = s:dfm_fg_line_'                . &background
        execute 'let s:dfm_fg          = s:dfm_fg_'                     . &background
        execute 'let s:dfm_fg_status   = s:dfm_fg_status_'              . &background
        execute 'let s:dfm_fg_user1    = s:dfm_fg_user1_'               . &background
        execute 'let s:dfm_fg_user2    = s:dfm_fg_user2_'               . &background
        execute 'let s:dfm_folded      = s:dfm_folded_'                 . &background
        execute 'let s:dfm_linenr_ins  = s:dfm_bg_'                     . &background
        execute 'let s:dfm_match       = s:dfm_match_'                  . &background
        execute 'let s:dfm_proof       = s:dfm_proof_'                  . &background
        execute 'let s:dfm_vsplit      = s:dfm_vsplit_'                 . &background
      endfunction

      " return (calculated) color variable value
      function! s:hexValue(varname)
        execute 'return ' . a:varname
      endfunction

    " ............................................................... Highlights

      function! s:highlights()
        Trace theme:highlights()
        let l:background = &background == 'light' ? 'dark' : 'light'
        let l:cursor     = s:hexValue('s:dfm_cursor_'   . l:background)
        let l:spell      = s:hexValue('s:dfm_bg_spell_' . &background)
        execute 'highlight CommandCursor   guibg=' . l:cursor        . ' guifg=' . s:dfm_bg
        execute 'highlight Cursor          guibg=' . s:dfm_cursor    . ' guifg=' . g:black
        execute 'highlight CursorLine      guibg=' . s:dfm_cursor_bg . ' guifg=' . s:dfm_cursorline
        execute 'highlight ErrorMsg        guibg=' . s:dfm_bg        . ' guifg=red'
        execute 'highlight ExtraWhitespace guibg=' . l:cursor        . ' guifg=' . s:hexValue('s:dfm_bg_' . l:background)
        execute 'highlight Folded          guibg=' . s:dfm_folded    . ' guifg=' . s:dfm_bg
        execute 'highlight InsertCursor    guibg=' . s:dfm_cursor    . ' guifg=' . s:dfm_bg
        execute 'highlight MatchParen      guibg=' . s:dfm_match     . ' guifg=' . s:dfm_bg . ' gui=bold'
        execute 'highlight ReplaceCursor   guibg=' . l:cursor        . ' guifg=' . s:dfm_bg
        execute 'highlight ShowMarksHLl    guibg=' . s:dfm_bg
        execute 'highlight SignColumn      guibg=' . s:dfm_bg
        execute 'highlight SpellBad        guibg=' . l:spell         . ' guifg=' . s:hexValue('s:dfm_fg_spell_' . &background)
        execute 'highlight User1           guibg=' . s:dfm_bg        . ' guifg=' . s:dfm_fg_user1
        execute 'highlight User2           guibg=' . s:dfm_bg        . ' guifg=' . s:dfm_fg_user2
        execute 'highlight VertSplit       guibg=' . s:dfm_vsplit    . ' guifg=' . s:dfm_vsplit
        execute 'highlight VisualCursor    guibg=' . l:cursor        . ' guifg=' . s:dfm_bg

        highlight! link SpellCap   SpellBad
        highlight! link SpellLocal SpellBad
        highlight! link SpellRare  SpellBad

        if &background == 'light' && g:lightscheme == 'flatwhite'  " add flatwhite contrast
          execute 'highlight IncSearch     guifg=' . g:light_fg . ' guibg=' . s:dfm_cursor . ' term=none cterm=none gui=none'
          execute 'highlight Search        guifg=' . g:white    . ' guibg=red guisp=red gui=bold'
          execute 'highlight StatuslineNC  guifg=' . g:white

          highlight link mkdLink    htmlString
          highlight link SneakScope Cursor
        endif
      endfunction

      " line numbers
      function! theme#LineNr()
        Trace theme#LineNr()
        execute                    'highlight CursorLineNr '  . (g:view == 0 ? 'gui=bold guifg=' . s:hexValue('s:dfm_bg_' . &background)
                                   \                                         : 'gui=none guifg=' . (b:view == 0 ? s:dfm_bg : s:dfm_fg_line))
        if mode() == 'n' | execute 'highlight LineNr  guifg=' . (g:view == 0 ? s:dfm_fg_line : s:dfm_bg)
        else             | execute 'highlight LineNr  guifg=' . s:dfm_linenr_ins | endif
        execute                    'highlight NonText guifg=red'
      endfunction

      " ruler, indents
      function! theme#Indent()
        Trace theme#Indent()
        execute                   'highlight IndentGuidesOdd  guibg=' . s:hexValue('s:dfm_bg_'        . &background)
        execute                   'highlight IndentGuidesEven guibg=' . s:hexValue('s:dfm_bg_line_'   . &background)
        if g:ruler == 2 | execute 'highlight ColorColumn      guibg=' . s:hexValue('s:dfm_bg_column_' . &background)
        else            | execute 'highlight ColorColumn      guibg=' . s:hexValue('s:dfm_bg_line_'   . &background) | endif

        if s:sync == 1  " refresh any indent guides, see theme#LiteSwitch()
          IndentGuidesToggle
          IndentGuidesToggle
          let s:sync = 0
        endif
      endfunction

      function! s:plugins()
        Trace theme:plugins()
        execute 'highlight ALEErrorSign   guifg=red gui=bold'
        execute 'highlight ALEWarningSign guifg=' . s:hexValue('s:dfm_ale_' . &background) . ' gui=bold'

        " g:fzf_colors initializes fzf only once, so override cursorline color
        let $FZF_DEFAULT_OPTS = '--reverse --color=fg+:' . s:hexValue('s:dfm_fg_' . &background)  " cannot appear to set other colors, such as hl+ (?)
        " hide bottom fzf window identifier
        execute 'highlight fzf1 guibg=' . s:dfm_bg . ' guifg=' . s:dfm_bg
        execute 'highlight fzf2 guibg=' . s:dfm_bg . ' guifg=' . s:dfm_bg
        execute 'highlight fzf3 guibg=' . s:dfm_bg . ' guifg=' . s:dfm_bg

        if &background == 'light'
          execute 'highlight SignifyLineAdd    guibg=' . s:dfm_bg . ' guifg=' . g:hue_3
          execute 'highlight SignifyLineChange guibg=' . s:dfm_bg . ' guifg=' . g:hue_2
          execute 'highlight SignifyLineDelete guibg=' . s:dfm_bg . ' guifg=' . g:hue_5_2
        else
          highlight link SignifyLineAdd    String
          highlight link SignifyLineChange Type
          highlight link SignifyLineDelete Identifier
        endif
        highlight   link SignifySignAdd    SignifyLineAdd
        highlight   link SignifySignChange SignifyLineChange
        highlight   link SignifySignDelete SignifyLineDelete
      endfunction

      " simple console theme tweaks to maximize transparency
      function! s:console()
        Trace theme:console()
        colorscheme pencil  " a theme that can be minimally tweaked for transparency
        set background=dark
        let $FZF_DEFAULT_OPTS = '--color=bg+:-1'  " fzf term transparency
        execute 'highlight fzf1         guibg=NONE guifg=#303030'
        execute 'highlight fzf2         guifg=NONE guifg=#303030'
        execute 'highlight fzf3         guibg=NONE guifg=#303030'
        execute 'highlight CursorLine   guibg=NONE ctermbg=NONE guifg=yellow gui=bold cterm=bold'
        execute 'highlight CursorLineNR guibg=NONE ctermbg=NONE'
        execute 'highlight FoldColumn   guibg=NONE'
        execute 'highlight LineNr       guibg=NONE'
        execute 'highlight Normal       guibg=NONE'
        execute 'highlight SignColumn   guibg=NONE'
        call theme#Margin()
        call s:noTilde()
      endfunction

  " Theme ______________________________________________________________________

    " ........................................................... Initialization
     
      function! theme#Theme()
        if ! g:gui | call s:console() | return | endif
        Trace theme#Theme()
        call s:highlights()
        call s:plugins()
        call theme#Indent()
        call theme#Margin()
        call s:noTilde()
        ColumnWrap
      endfunction

    " ............................................................ Switch colour

      function! theme#ColorScheme(contrast)
        Trace theme#ColorScheme()
        if a:contrast == 0
          let &background = 'light'  " must set background before colorscheme for flatwhite colors
          execute 'colorscheme ' . g:lightscheme
        else
          let &background = 'dark'
          colorscheme quantum
        endif
      endfunction

      " toggle colour scheme
      function! theme#LiteSwitch()
        Trace theme#LiteSwitch()
        Quietly LiteDFMClose  " trap and ignore initialization error
        if &background == 'light' | call theme#ColorScheme(1)
        else                      | call theme#ColorScheme(0) | endif
        let s:sync = 1  " see theme#Indent()
        call ui#LiteType()
      endfunction

  " Font _______________________________________________________________________

    " .......................................................... Balance margins

      " balance left right margins with font size changes (and window resizing)
      function! theme#Margin()
        Trace theme#Margin()
        let g:lite_dfm_left_offset = max([1, min([22, (&columns - &textwidth - 4) / 2])])  " account for linenr <space> text
        Quietly LiteDFM
        call theme#LineNr()
        call ui#RefreshInfo()
      endfunction

    " ................................................................. Set font

      function! theme#Font(size)
        Trace theme#Font()
        execute 'set guifont=' . (core#Prose() ? g:prose_font : g:source_font) . ' ' . a:size
        if exists('s:size')  " font size change redraw
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
            set laststatus=2  " turn on statusline
          endif
        endif
      endfunction

      function! s:fontSwitch()
        Trace theme:fontSwitch()
        call theme#FontSize(g:font_type == 1 ? 0 : 1)
        if ! core#Prose()
          Quietly LiteDFMClose
          call ui#LiteType()
        endif
      endfunction

  "  Distraction free highlight ________________________________________________

    " ................................................................ Code view

      function! theme#CodeView()
        Trace theme#CodeView()
        execute 'highlight LineNr guifg=' . s:dfm_fg_line
      endfunction

    " .................................................... Distraction free view

      " prose style
      function! theme#DfmView()
        Trace theme#DfmView()
        if core#Prose() | execute 'highlight CursorLine gui=none guibg=' . s:dfm_bg . ' guifg=' . s:dfm_fg | endif
      endfunction

    " .............................................................. Switch View

      " toggle full document highlight
      function! theme#ToggleProof()
        execute 'let g:limelight_conceal_guifg="' . s:dfm_proof . '"'
      endfunction

    " .............................................................. EOF markers

      " hide tilde marker (not applicable to console)
      function! s:noTilde()
        if $DISPLAY > ''
          execute 'highlight EndOfBuffer ctermfg=black guifg=' . s:dfm_bg
          " reset menu highlight after loading autocompletion plugin
          execute 'highlight PmenuSel term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=' . s:dfm_bg_line . ' guibg=' . s:dfm_bg
          " match command line tab menu
          execute 'highlight WildMenu term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=' . s:dfm_bg_line . ' guibg=' . s:dfm_bg
        endif
      endfunction

  " Context highlight __________________________________________________________

    " ............................................................... Prose mode
     
      " enhanced limelight contrast, see ui#ToggleProof()
      function! theme#Contrast(level)
        Trace theme#Contrast()
        if core#Prose() && &background == 'light'
          if a:level
            execute 'highlight! Normal     guifg=' . g:mono_3
            execute 'highlight! CursorLine guifg=' . g:black
          else
            execute 'highlight! Normal     guifg=' . g:mono_2
            execute 'highlight! CursorLine guifg=' . g:mono_2
          endif
        endif
      endfunction

    " ............................................................... Statusline

      " undo statusline gui=reverse
      function! theme#ShowStatusline()
        execute 'highlight Statusline gui=none guibg=' . s:dfm_bg_status . ' guifg=' . s:dfm_fg_status
        set laststatus=2
      endfunction

      function! theme#ShowInfo()
        execute 'highlight Statusline guibg=' . s:dfm_bg
      endfunction

" theme.vim
