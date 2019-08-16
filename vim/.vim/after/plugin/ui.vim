" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

  " Layout _____________________________________________________________________

    " .................................................................... Setup

      let g:pad_inner    = '    '  " statusline padding
      let g:pad_outer    = '   '   " expanded statusline padding
      let g:view         = 1       " initial view mode (0) info (1) df
      let s:expanded     = 0       " statusline state (0) dfm (1) expanded
      let s:font_changed = 0       " redraw flag
      let s:show         = 1       " statusline (0) off (1) on

  "  Distraction free modes ____________________________________________________

    " ................................................................ Code view

      " source code style
      function! s:codeView()
        Trace ui:codeView()
        let g:view = 0
        " syntax enable  " restore CursorLine syntax highlighting before applying themes
        if exists('g:loaded_limelight') | Limelight! | endif
        Theme
        ShowStatusLine
        CodeView
        set showmode
      endfunction

    " .................................................... Distraction free view

      " prose style
      function! s:proseView()
        Trace ui:proseView()
        let g:view = 1
        " silent !tmux set status off
        DfmView
        if Prose() || g:ruler == 0 | set colorcolumn=0 | endif
        set foldcolumn=0
        set laststatus=0
        set noshowmode
        set scrolloff=8
        if Prose() | set spell
        else       | set nospell | endif
        call s:view()
      endfunction

    " .............................................................. Switch View

      " toggle full document highlight
      function! s:view()
        Trace ui:view()
        let l:col = virtcol('.')
        Theme
        if b:view == 1  " proof view
          " call s:showInfo(1)
          Limelight!
          Contrast 0
        else            " dfm view
          " call s:showInfo(0)
          Limelight
          Contrast 1
        endif
        execute 'normal! ' . l:col . '|'
      endfunction

      function! ToggleProof()
        Trace ui:ToggleProof()
        if CommandWindow() | return | endif
        " if Prose() | let b:view = b:view == 0 ? 1 : 0 | endif
        let b:view = b:view == 0 ? 1 : 0
        call s:view()
      endfunction

      function! s:setView()
        Trace ui:setView()
        if g:view == 0 | call s:codeView()
        else           | call s:proseView() | endif
      endfunction

      " toggle dfm view
      function! s:switchView()
        Trace ui:SwitchView
        let l:col = col('.')
        let g:view = g:view == 0 ? 1 : 0
        call s:setView()
        execute 'normal! ' . l:col . '|'
      endfunction
      
      command! SwitchView silent! call <SID>switchView()

  " Screen focus _______________________________________________________________

    " ........................................................... Screen display

      " initial view
      function! LiteType()
        Trace ui:LiteType()
        call Font(Prose())
        Palette
        if ! exists('b:view') | let b:view = 1 | endif  " initial view (proof)
        call s:setView()
      endfunction

      " redraw
      function! Retheme()
        Trace ui:Retheme()
        let lstatus     = &laststatus
        call LiteType()   
        let &laststatus = lstatus
      endfunction

    " .......................................................... Balance margins

      function! Offset()
        return max([1, min([22, (&columns - &textwidth - 4) / 2])])  " account for linenr <space> text
      endfunction

      " balance left right margins with font size changes (and window resizing)
      function! Margins()
        Trace ui:Margin
        let g:lite_dfm_left_offset = Offset()
        Quietly LiteDFM
        LineNr
        RefreshInfo
      endfunction

    " ................................................................. Set font

      " adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
      function! Font(type)
        Trace ui:Font()
        if $DISPLAY > ''
          if g:font_type != a:type
            let g:font_type = a:type
            let l:size      = system('fontsize')
            let l:size      = a:type == 0 ? l:size : l:size + g:font_step
            execute 'set guifont=' . (Prose() ? g:prose_font : g:source_font) . ' ' . l:size
            if s:font_changed
              RedrawGui
            endif
            let s:font_changed = 1  " next font size change requires redraw
            set laststatus=2        " turn on statusline
          endif
        endif
      endfunction


  " Context statusline _________________________________________________________

    " ........................................................ Statusline format

      function! Detail()
        let l:prefix = g:detail == 0 ? Tag() : Atom()
        return l:prefix > '' ? l:prefix . '  ' . SpecialChar() : SpecialChar()
      endfunction

      " [path] .. filename | pos .. [details]
      function! s:statusline(proof)
        " Trace ui:statusline()  " tmi :-)
        try  " trap snippet insertion interruption
          let g:prose = 1
          if Prose() && a:proof == 0
            return Escape(Leader('') . '  %{UnModified(0)}%*')
          else
            let l:name     = '%{Name()}' . g:pad_inner
            if s:expanded == 0  " center dfm indicator / proofing statusline
              let l:leader = '%{Leader(Name())}'
            else
              let l:path   = '%{Path()}'
              let l:leader = '%{Leader(Path() . g:pad_outer . Name())}'
            endif
            let l:name     = '%1*' . l:name
            let l:info     = '%{UnModified(1)}' . g:pad_inner . ' ' . '%{PosWordsCol()}'  " utf-8 symbol occupies 2 chars (pad right 1 space)
            if s:expanded == 1
              let l:name   = '%2*' . l:path . '%1*' . g:pad_outer . l:name
              let l:info  .= g:pad_outer . '%2*%{Detail()}'
            endif
            return Escape('%1*' . l:leader . l:name . l:info . '%1*')
          endif
        catch /.*/  " discard messages
        endtry
      endfunction

    " .......................................................... Show statusline

      function! s:showInfo(proof)
        Trace ui:showInfo()
        if a:proof == 1 || !Prose()
          execute 'set statusline=' . s:statusline(a:proof)
          ShowStatusLine
        else
          HideInfo
        endif
      endfunction

      function! s:refreshInfo()
        Trace ui:RefreshInfo
        call s:showInfo(b:view)
      endfunction

      command! RefreshInfo silent! call <SID>refreshInfo()

      function! s:toggleInfo(...)
        Trace ui:ToggleInfo
        if a:0 && a:1 | return | endif  " prose insert mode is always dfm
        let l:col = col('.')
        let s:expanded = (s:expanded == 0 ? 1 : 0)
        " if Prose() | call ToggleProof()  " toggle between writing and proofing modes
        " else       | call s:showInfo(b:view) | endif
        call s:showInfo(b:view)
        execute 'normal! ' . l:col . '|'
      endfunction

      command! -nargs=? ToggleInfo silent! call <SID>toggleInfo(<f-args>)

" ui.vim
