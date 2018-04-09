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
      let s:dfm_proof_light     = s:rgb_10  " dark foreground
      let s:dfm_proof_dark      = s:gray5   " light foreground

      " background
      let s:dfm_bg_light        = s:rgb_15  " solarized light (paper) background
      let s:dfm_bg_dark         = s:gray1   " quantum dark background
      let s:dfm_vsplit_light    = s:rgb_15  " invisible split
      let s:dfm_vsplit_dark     = s:gray1   " invisible split

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

    " .................................................................. Palette

      function! ui#Palette()
        call core#Trace('ui#Palette()')
        execute 'let s:dfm_fg          = g:dfm_fg_'                     . &background
        execute 'let s:dfm_proof       = s:dfm_proof_'                  . &background
        execute 'let g:dfm_bg          = s:dfm_bg_'                     . &background
        execute 'let s:dfm_vsplit      = s:dfm_vsplit_'                 . &background
        execute 'let s:dfm_cursor      = s:dfm_cursor_'                 . &background
        execute 'let s:dfm_bg_line     = s:dfm_bg_line_'                . &background
        execute 'let s:dfm_fg_line     = s:dfm_fg_line_'                . &background
        execute 'let s:dfm_bg_status   = s:dfm_bg_status_'              . &background
        execute 'let s:dfm_fg_status   = s:dfm_fg_status_'              . &background
        execute 'let s:dfm_cursorline  = s:cursorline == 0 ? s:dfm_bg_' . &background . ' : s:dfm_bg_line_' . &background
        execute 'let g:dfm_linenr_ins  = s:dfm_bg_'                     . &background
      endfunction

    " ............................................................. Colour theme

      function! ui#color(color)
        execute 'return ' . a:color
      endfunction

      " margins, selection and cursor
      function! ui#Theme()
        call core#Trace('ui#Theme()')
        let l:background = &background == 'light' ? 'dark' : 'light'
        execute 'highlight ExtraWhitespace     guibg=' . ui#color('s:dfm_cursor_'  . l:background) . ' guifg=' . ui#color('s:dfm_bg_' . l:background)
        execute 'highlight VisualCursor        guibg=' . ui#color('s:dfm_cursor_'  . l:background) . ' guifg=' . g:dfm_bg
        execute 'highlight ReplaceCursor       guibg=' . ui#color('s:dfm_cursor_'  . l:background) . ' guifg=' . g:dfm_bg
        execute 'highlight CommandCursor       guibg=' . ui#color('s:dfm_cursor_'  . l:background) . ' guifg=' . g:dfm_bg
        execute 'highlight IndentGuidesEven    guibg=' . ui#color('s:dfm_bg_line_' . l:background)
        execute 'highlight IndentGuidesOdd     guibg=' . ui#color('s:dfm_bg_'      . &background)
        execute 'highlight VertSplit           guibg=' . s:dfm_vsplit                              . ' guifg=' . s:dfm_vsplit
        execute 'highlight ShowMarksHLl        guibg=' . g:dfm_bg
        execute 'highlight SignColumn          guibg=' . g:dfm_bg
        execute 'highlight InsertCursor        guibg=' . s:dfm_cursor                              . ' guifg=' . g:dfm_bg
        execute 'highlight CursorLine gui=none guibg=' . s:dfm_cursorline
        execute 'highlight Cursor gui=bold     guibg=' . s:dfm_cursor                              . ' guifg=' . g:dfm_bg
        execute 'highlight ALEErrorSign        guifg=' . s:dfm_fg
        execute 'highlight ALEWarningSign      guifg=' . s:dfm_cursor
        call FzfColors()                    " see setting.vim
        call ui#IndentTheme()
        call ui#Margin()
        call ui#LineNr()
        call core#NoTilde()
      endfunction

      function! ui#LineNr()
        call core#Trace('ui#LineNr()')
        execute 'highlight CursorLineNr ' . (g:view == 0 ? 'gui=bold guifg=' . ui#color('g:dfm_fg_' . &background)
                \                                        : 'gui=none guifg=' . (b:proof == 0 ? g:dfm_bg : s:dfm_fg_line))
        let g:dfm_linenr_cmd = g:view == 0  ? s:dfm_fg_line  : g:dfm_bg
      endfunction

      function! ui#IndentTheme()
        call core#Trace('ui#IndentTheme()')
        execute 'highlight IndentGuidesOdd  guibg=' . ui#color('s:dfm_bg_'        . &background)
        execute 'highlight IndentGuidesEven guibg=' . ui#color('s:dfm_bg_line_'   . &background)
        if g:ruler == 2
          execute 'highlight ColorColumn    guibg=' . ui#color('s:dfm_bg_column_' . &background)
        else
          execute 'highlight ColorColumn    guibg=' . ui#color('s:dfm_bg_line_'   . &background)
        endif
        if s:sync == 1                      " refresh any indent guides, see ui#LiteSwitch()
          execute 'IndentGuidesToggle'
          execute 'IndentGuidesToggle'
          let s:sync = 0
        endif
      endfunction

      function! ui#Retheme()
        call core#Trace('ui#Refresh()')
        let lstatus     = &laststatus
        call ui#SetView()
        let &laststatus = lstatus
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

      " balance left right margins with font size changes
      function! ui#Margin()
        call core#Trace('ui#Margin()')
        let g:lite_dfm_left_offset = max([1, min([22, (&columns - &textwidth) / 2])])
        call core#Quietly('LiteDFM')
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

      function! ui#LiteLine()
        call core#Trace('ui#LiteLine()')
        if g:view == 0
          " disable lightline to allow new settings to be loaded
          call lightline#disable()
          call LightLine()                  " see setting.vim
          " match lightline to current colorscheme
          " see https://github.com/itchyny/lightline.vim/issues/104
          " force load with different colorscheme name (only difference to solarized_light)
          execute "let g:lightline.colorscheme = 'solarized_" . &background . "'"
          call lightline#init()
          call lightline#colorscheme()
          call lightline#update()
          call lightline#enable()
        endif
        set laststatus=2                    " turn on statusline
      endfunction

      " source code style
      function! ui#CodeView()
        call core#Trace('ui#CodeView()')
        let g:view = 0
        " restore CursorLine syntax highlighting before applying themes
        syntax enable
        if exists('g:loaded_limelight')
          execute 'Limelight!'
        endif
        call ui#Theme()
        call ui#LiteLine()
        execute 'highlight LineNr guifg=' . s:dfm_fg_line
        set showmode
      endfunction

    " .................................................... Distraction free view

      " vimwiki prose style
      function! ui#DfmView(...)
        call core#Trace('ui#DfmView()')
        let g:view  = 1
        " silent !tmux set status off
        " set numberwidth=1                 " goyo settings
        " set nonumber
        " set fillchars-=stl:.              " remove statusline fillchars '.' set by goyo.vim
        " set fillchars+=stl:\ "
        if &filetype == 'vimwiki'
          call VimwikiLink()                " restore vimwiki link
        endif
        execute 'highlight CursorLine gui=none guibg=' . g:dfm_bg . ' guifg=' . s:dfm_fg
        set colorcolumn=0
        set foldcolumn=0
        set laststatus=0
        set noshowmode
        set scrolloff=8
        if core#Prose()
          set spell
        else
          set nospell
        endif
        if !a:0
          " initialize view mode (negate toggle)
          let b:proof = b:proof == 0 ? 1 : 0
          call ui#ToggleProof()
        endif
      endfunction

      " toggle full document highlight
      function! ui#ToggleProof()
        call core#Trace('ui#ToggleProof()')
        let b:proof = b:proof == 0 ? 1 : 0
        call ui#Theme()
        execute 'highlight Normal guifg=' . s:dfm_proof
        if b:proof == 1
          if &filetype == 'vimwiki'
            " hack to apply more robust markdown syntax highlighting
            " note: this hack may break vimwiki navigation in the future!
            set filetype=markdown
            call ui#DfmView(0)
            " force margin centering
            call ui#Retheme()
          endif
          call ui#ShowInfo(1)
          execute 'Limelight!'
        else
          call ui#ShowInfo(0)
          execute 'Limelight'
        endif
      endfunction

    " .............................................................. Switch View

      function! ui#SetView()
        call core#Trace('ui#SetView()')
        if g:view == 0
          call ui#CodeView()
        else
          call ui#DfmView()
        endif
      endfunction

      function! ui#SwitchView()
        call core#Trace('ui#SwitchView()')
        let l:col = col('.')
        let g:view = g:view == 0 ? 1 : 0
        call ui#SetView()
        execute 'normal! ' . l:col . '|'
      endfunction

  " Screen focus ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Screen display

      function! ui#LiteType()
        call core#Trace('ui#LiteType()')
        call ui#FontSize(core#Prose() ? 1 : 0)
        call ui#Palette()
        if !exists('b:proof')
          let b:proof = s:initial_view
        endif
        if exists('g:view')
          call ui#SetView()
        else
          call ui#DfmView()                 " the default startup view
        endif
      endfunction

  " Enhanced statusline ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Statusline format

      " center dfm indicator / proofing statusline
      function! ui#WikiInfo(proof)
        " call core#Trace('ui#WikiInfo()')
        try                                 " trap snippet insertion interruption
          let g:prose = 1
          if a:proof == 0
            let l:info = (&modified ? g:modified_ind : '')
            let l:leader = repeat(' ', (winwidth(0) - strlen(l:name)) / 2 + 2)
            return l:leader . l:info
          else
            let l:name = expand('%:t' . (core#Prose() ? ':r' : '')) . '   '
            " fixed center point on file status
            let l:leader = repeat(' ', (winwidth(0) / 2) - strlen(l:name))
            let l:info = (&modified ? g:modified_ind : g:unmodified_ind) . '   ' . (core#Prose() ? info#WordCount() : col('.'))
            return l:leader . l:name . l:info
          endif
        catch
        endtry
      endfunction

      function! ui#ShowInfo(proof)
        call core#Trace('ui#ShowInfo()')
        call lightline#disable()
        if s:wikiinfo == 1
          execute 'set statusline=%{ui#WikiInfo(' . a:proof . ')}'
          " undo statusline gui=reverse
          execute 'highlight statusline gui=none guibg=' . s:dfm_bg_status . ' guifg=' . s:dfm_fg_status
          set laststatus=2
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

      function! ui#ToggleInfo()
        call core#Trace('ui#ToggleInfo()')
        if core#Prose()                     " toggle between writing and proofing modes
          call ui#ToggleProof()
        else
          call ui#CodeView()                " refresh margin
          let g:code = (g:code == 0 ? 1 : 0)
        endif
      endfunction

" ui.vim
