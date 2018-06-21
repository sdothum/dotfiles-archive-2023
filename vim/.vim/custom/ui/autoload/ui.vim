" sdothum - 2016 (c) wtfpl

" User Interface
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Theme ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let g:font_type           = -1        " current font setting (0) source (1) prose
      let s:font_step           = 1         " in point size
      let s:cursorline          = 0         " visible (0) off (1) on
      let s:wikiinfo            = 1         " statusline (0) off (1) on
      let s:initial_view        = 1         " prose (0) dfm (1) proof
      let s:sync                = 0         " sync (0) off (1) indent guides
      let s:info                = 0         " statusline (0) dfm (1) expanded
      let g:padding             = '   '     " expanded statusline padding

      " Iosevka custom compiled, with nerd-fonts awesome patches, see make_install/iosevka
      let s:source_font         = 'Iosevka\'
      let s:prose_font          = 'Iosevka-proof\'

    " .............................................................. Color codes

      " solarized colour palette (light)
      let s:rgb_0               = '#073642' " base02 dark highlight
      let s:rgb_1               = '#dc322f' " red
      let s:rgb_2               = '#719e07' " green
      let s:rgb_3               = '#b58900' " yellow
      let s:rgb_4               = '#268bd2' " blue
      let s:rgb_5               = '#d33682' " magenta
      let s:rgb_6               = '#2aa198' " cyan
      let s:rgb_7               = '#eee8d5' " base2 light highlight
      let s:rgb_8               = '#002b36' " base03 dark bg
      let s:rgb_9               = '#cb4b16' " orange
      let s:rgb_10              = '#586e75' " base01 darkest grey
      let s:rgb_11              = '#657b83' " base00 dark grey
      let s:rgb_12              = '#839496' " base0 light grey
      let s:rgb_13              = '#6c71c4' " violet
      let s:rgb_14              = '#93a1a1' " base1 lightest grey
      let s:rgb_15              = '#fdf6e3' " base3 light bg

      " quantum colour palette (dark)
      let s:gray1               = '#263238' " 023 (005f5f)
      let s:gray2               = '#2c3a41' " 023 (005f5f)
      let s:gray3               = '#425762' " 059 (5f5f5f)
      let s:gray4               = '#658494' " 066 (5f8787)
      let s:gray5               = '#aebbc5' " 146 (afafd7)
      let s:blue                = '#70ace5' " 074 (5fafd7)
      let s:cyan                = '#69c5ce' " 080 (5fd7d7)
      let s:green               = '#87bb7c' " 108 (87af87)
      let s:indigo              = '#7681de' " 104 (8787d7)
      let s:orange              = '#d7956e' " 173 (d7875f)
      let s:purple              = '#a48add' " 140 (af87d7)
      let s:red                 = '#dd7186' " 168 (d75f87)
      let s:yellow              = '#d5b875' " 180 (d7af87)

    " ......................................................... DFM colour masks

      " foreground
      let g:dfm_fg_light        = '#000000' " light foreground (high contrast)
      let g:dfm_fg_dark         = '#ffffff' " dark foreground (high contrast)
      let s:dfm_proof_light     = '#dddddd' " dark foreground
      let s:dfm_proof_dark      = '#444444' " light foreground
      let s:dfm_ale_light       = s:rgb_1
      let s:dfm_ale_dark        = s:red

      " background
      let s:dfm_bg_light        = s:rgb_15  " solarized light (paper) background
      let s:dfm_bg_dark         = s:gray1   " quantum dark background
      let s:dfm_vsplit_light    = s:rgb_15  " invisible split
      let s:dfm_vsplit_dark     = s:gray1   " invisible split
      let s:dfm_folded_light    = s:gray5   " vimdiff fold
      let s:dfm_folded_dark     = s:gray4   " vimdiff fold

      " cursor line
      let s:dfm_cursor_light    = '#20bbfc' " iA Writer
      let s:dfm_cursor_dark     = '#20bbfc' " iA Writer
      let s:dfm_bg_line_light   = s:rgb_7   " light cursorline
      let s:dfm_bg_line_dark    = s:gray2   " dark cursorline
      let s:dfm_bg_column_light = '#E2D7B6' " light column (darker shade of rgb_7)
      let s:dfm_bg_column_dark  = '#0A4C5C' " dark column (lighter shade of rgb_0)
      let s:dfm_fg_line_light   = s:rgb_14  " light line numbers
      let s:dfm_fg_line_dark    = s:gray4   " dark line numbers

      " statusline
      let s:dfm_bg_status_light = s:rgb_15  " light statusline
      let s:dfm_bg_status_dark  = s:gray1   " dark statusline
      let s:dfm_fg_status_light = s:rgb_0   " light statusline
      let s:dfm_fg_status_dark  = s:rgb_7   " dark statusline
      let s:dfm_fg_user1_light  = s:rgb_0   " light statusline
      let s:dfm_fg_user1_dark   = s:rgb_7   " dark statusline
      let s:dfm_fg_user2_light  = s:rgb_4   " light statusline
      let s:dfm_fg_user2_dark   = s:cyan    " dark statusline

    " .................................................................. Palette

      function! ui#Palette()
        call core#Trace('ui#Palette()')
        execute 'let s:dfm_fg          = g:dfm_fg_'                     . &background
        execute 'let s:dfm_proof       = s:dfm_proof_'                  . &background
        execute 'let g:dfm_bg          = s:dfm_bg_'                     . &background
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
      function! ui#Value(varname)
        execute 'return ' . a:varname
      endfunction

    " ............................................................. Colour theme

      " margins, selection and cursor
      function! ui#Theme()
        call core#Trace('ui#Theme()')
        let l:background = &background == 'light' ? 'dark' : 'light'
        execute 'highlight ExtraWhitespace     guibg=' . ui#Value('s:dfm_cursor_'    . l:background) . ' guifg=' . ui#Value('s:dfm_bg_' . l:background)
        execute 'highlight VisualCursor        guibg=' . ui#Value('s:dfm_cursor_'    . l:background) . ' guifg=' . g:dfm_bg
        execute 'highlight ReplaceCursor       guibg=' . ui#Value('s:dfm_cursor_'    . l:background) . ' guifg=' . g:dfm_bg
        execute 'highlight CommandCursor       guibg=' . ui#Value('s:dfm_cursor_'    . l:background) . ' guifg=' . g:dfm_bg
        execute 'highlight Folded              guibg=' . s:dfm_folded                                . ' guifg=' . g:dfm_bg
        execute 'highlight User1               guibg=' . g:dfm_bg                                    . ' guifg=' . s:dfm_fg_user1
        execute 'highlight User2               guibg=' . g:dfm_bg                                    . ' guifg=' . s:dfm_fg_user2
        execute 'highlight VertSplit           guibg=' . s:dfm_vsplit                                . ' guifg=' . s:dfm_vsplit
        execute 'highlight ShowMarksHLl        guibg=' . g:dfm_bg
        execute 'highlight SignColumn          guibg=' . g:dfm_bg
        execute 'highlight InsertCursor        guibg=' . s:dfm_cursor                                . ' guifg=' . g:dfm_bg
        execute 'highlight CursorLine gui=none guibg=' . s:dfm_cursorline
        execute 'highlight Cursor gui=bold     guibg=' . s:dfm_cursor                                . ' guifg=' . g:dfm_bg
        execute 'highlight ALEErrorSign        guifg=' . ui#Value('s:dfm_ale_'    . l:background)
        execute 'highlight ALEWarningSign      guifg=' . s:dfm_fg
        call FzfColors()                    " see setting.vim
        call ui#IndentTheme()
        call ui#Margin()
        call core#NoTilde()
      endfunction

      " ruler, indents
      function! ui#IndentTheme()
        call core#Trace('ui#IndentTheme()')
        execute 'highlight IndentGuidesOdd     guibg=' . ui#Value('s:dfm_bg_'        . &background)
        execute 'highlight IndentGuidesEven    guibg=' . ui#Value('s:dfm_bg_line_'   . &background)
        if g:ruler == 2
          execute 'highlight ColorColumn       guibg=' . ui#Value('s:dfm_bg_column_' . &background)
        else
          execute 'highlight ColorColumn       guibg=' . ui#Value('s:dfm_bg_line_'   . &background)
        endif
        if s:sync == 1                      " refresh any indent guides, see ui#LiteSwitch()
          execute 'IndentGuidesToggle'
          execute 'IndentGuidesToggle'
          let s:sync = 0
        endif
      endfunction

      " line numbers
      function! ui#LineNr(mode)
        call core#Trace('ui#LineNr()')
        execute 'highlight CursorLineNr ' . (s:view == 0 ? 'gui=bold guifg=' . ui#Value('g:dfm_fg_' . &background)
                \                                        : 'gui=none guifg=' . (b:proof == 0 ? g:dfm_bg : s:dfm_fg_line))
        let s:dfm_linenr_cmd = s:view == 0  ? s:dfm_fg_line  : g:dfm_bg
        if a:mode == 'n'
          execute 'highlight LineNr guifg=' . s:dfm_linenr_cmd
        else
          execute 'highlight LineNr guifg=' . s:dfm_linenr_ins
        endif
      endfunction

    " Contrast ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Switch colour

      " toggle colour scheme
      function! ui#LiteSwitch()
        call core#Trace('ui#LiteSwitch()')
        " trap and ignore initialization error
        call core#Quietly('LiteDFMClose')
        if &background == 'light'
          let &background = 'dark'
          colorscheme quantum
        else
          let &background = 'light'
          colorscheme solarized8_high
        endif
        let s:sync = 1                      " see ui#IndentTheme()
        call ui#LiteType()
      endfunction

  " Font ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch font

      " balance left right margins with font size changes (and window resizing)
      function! ui#Margin()
        call core#Trace('ui#Margin()')
        " account for linenr <space> text
        let g:lite_dfm_left_offset = max([1, min([22, (&columns - &textwidth - 4) / 2])])
        call core#Quietly('LiteDFM')
        call ui#LineNr(mode())
        call ui#RefreshInfo()
      endfunction

      function! ui#Font(size)
        call core#Trace('ui#Font()')
        execute 'set guifont='   . (core#Prose() ? s:prose_font : s:source_font)  . ' ' . a:size
        if exists('s:size')
          call core#RedrawGui()             " gui redrawi for font size change
        endif
        let s:size = a:size
      endfunction

      " adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
      function! ui#FontSize(type)
        call core#Trace('ui#FontSize()')
        if $DISPLAY > ''
          if g:font_type != a:type
            let g:font_type = a:type
            let l:size      = system('fontsize')
            call ui#Font(a:type == 0 ? l:size : l:size + s:font_step)
            set laststatus=2                " turn on statusline
          endif
        endif
      endfunction

      function! ui#FontSwitch()
        call core#Trace('ui#FontSwitch()')
        call ui#FontSize(g:font_type == 1 ? 0 : 1)
        if !core#Prose()
          call core#Quietly('LiteDFMClose')
          call ui#LiteType()
        endif
      endfunction

  "  Distraction free modes ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Code view

      " source code style
      function! ui#CodeView()
        call core#Trace('ui#CodeView()')
        let s:view = 0
        " restore CursorLine syntax highlighting before applying themes
        " syntax enable
        if exists('g:loaded_limelight')
          execute 'Limelight!'
        endif
        call ui#Theme()
        call ui#ShowStatusline()
        execute 'highlight LineNr guifg=' . s:dfm_fg_line
        set showmode
      endfunction

    " .................................................... Distraction free view

      " prose style
      function! ui#DfmView()
        call core#Trace('ui#DfmView()')
        let s:view = 1
        " silent !tmux set status off
        " un/comment to have monochromatic cursor line (looses vimdiff highlighting)
        execute 'highlight CursorLine gui=none guibg=' . g:dfm_bg . ' guifg=' . s:dfm_fg
        if core#Prose() || g:ruler == 0
          set colorcolumn=0
        endif
        set foldcolumn=0
        set laststatus=0
        set noshowmode
        set scrolloff=8
        if core#Prose()
          set spell
        else
          set nospell
        endif
        " initialize view mode (negate toggle)
        let b:proof = b:proof == 0 ? 1 : 0
        call ui#ToggleProof()
      endfunction

    " .............................................................. Switch View

      " toggle full document highlight
      function! ui#ToggleProof()
        call core#Trace('ui#ToggleProof()')
        let b:proof = b:proof == 0 ? 1 : 0
        call ui#Theme()
        if core#Prose()
          execute 'let g:limelight_conceal_guifg="' . s:dfm_proof . '"'
        endif
        if b:proof == 1
          call ui#ShowInfo(1)
          execute 'Limelight!'
        else
          call ui#ShowInfo(0)
          execute 'Limelight'
        endif
      endfunction

      function! ui#SetView()
        call core#Trace('ui#SetView()')
        if s:view == 0
          call ui#CodeView()
        else
          call ui#DfmView()
        endif
      endfunction

      " toggle dfm view
      function! ui#SwitchView()
        call core#Trace('ui#SwitchView()')
        let l:col = col('.')
        let s:view = s:view == 0 ? 1 : 0
        call ui#SetView()
        execute 'normal! ' . l:col . '|'
      endfunction

  " Screen focus ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Screen display

      " initial view
      function! ui#LiteType()
        call core#Trace('ui#LiteType()')
        call ui#FontSize(core#Prose() ? 1 : 0)
        call ui#Palette()
        if !exists('b:proof')
          let b:proof = s:initial_view
        endif
        if exists('s:view')
          call ui#SetView()
        else
          call ui#DfmView()                 " the default startup view
        endif
      endfunction

      " redraw
      function! ui#Retheme()
        call core#Trace('ui#Refresh()')
        let lstatus     = &laststatus
        call ui#SetView()
        let &laststatus = lstatus
      endfunction

  " Context statusline ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Statusline format

      " center dfm indicator / proofing statusline
      function! ui#WikiInfo(proof)
        " call core#Trace('ui#WikiInfo()')
        try                                 " trap snippet insertion interruption
          let g:prose = 1
          if core#Prose() && a:proof == 0
            return info#Escape(info#Leader('') . '  %{info#UnModified(0)}%*')
          else
            let l:name     = '%{info#Name()}   '
            if s:info == 0
              let l:leader = '%{info#Leader(info#Name())}'
            else
              let l:path   = '%{info#Path()}'
              let l:leader = '%{info#Leader(info#Path() . g:padding . info#Name())}'
            endif
            let l:name     = '%1*' . l:name
            let l:info     = '%{info#UnModified(1)}   %{info#PosWordsCol()}%*'
            if s:info == 1
              let l:name   = '%2*' . l:path . '%*' . g:padding . l:name
              let l:info   = l:info . g:padding . '%2*%{info#Atom()}  %{info#SpecialChar()}%*'
            endif
            return info#Escape(l:leader . l:name . l:info)
          endif
        catch
        endtry
      endfunction

      function! ui#ShowStatusline()
        " undo statusline gui=reverse
        execute 'highlight statusline gui=none guibg=' . s:dfm_bg_status . ' guifg=' . s:dfm_fg_status
        set laststatus=2
      endfunction

      function! ui#ShowInfo(proof)
        call core#Trace('ui#ShowInfo()')
        if s:wikiinfo == 1
          " execute 'set statusline=%{ui#WikiInfo(' . a:proof . ')}'
          execute 'set statusline=' . ui#WikiInfo(a:proof)
          call ui#ShowStatusline()
        else
          " simply hide statusline content
          execute 'highlight statusline guibg=' . g:dfm_bg
        endif
      endfunction

      function! ui#RefreshInfo()
        call core#Trace('ui#RefreshInfo()')
        call ui#ShowInfo(b:proof)
      endfunction

    " ........................................................ Toggle statusline

      function! ui#ToggleInfo(...)
        call core#Trace('ui#ToggleInfo()')
        if a:0                              " exiting insert mode? see plugin/ui.vim autocmd
          if b:proof == s:initial_view      " already default view?
            return
          endif
        endif
        let l:col = col('.')
        let s:info = (s:info == 0 ? 1 : 0)
        if core#Prose()                     " toggle between writing and proofing modes
          call ui#ToggleProof()
        else
          call ui#ShowInfo(b:proof)
        endif
        execute 'normal! ' . l:col . '|'
      endfunction

" ui.vim
