" sdothum - 2016 (c) wtfpl

" User Interface
" ══════════════════════════════════════════════════════════════════════════════

  " Layout _____________________________________________________________________

    " .................................................................... Setup

      let s:show         = 1  " statusline (0) off (1) on
      let s:initial_view = 1  " startup in (0) dfm (1) proof view
      let s:expanded     = 0  " statusline state (0) dfm (1) expanded

  "  Distraction free modes ____________________________________________________

    " ................................................................ Code view

      " source code style
      function! s:codeView()
        Trace ui:codeView()
        let g:view = 0
        " syntax enable  " restore CursorLine syntax highlighting before applying themes
        if exists('g:loaded_limelight') | Limelight! | endif
        call theme#Theme()
        call theme#ShowStatusline()
        call theme#CodeView()
        set showmode
      endfunction

    " .................................................... Distraction free view

      " prose style
      function! s:dfmView()
        Trace ui:dfmView()
        let g:view = 1
        " silent !tmux set status off
        call theme#DfmView()  " un/comment to have monochromatic cursor line (looses vimdiff highlighting)
        if core#Prose() || g:ruler == 0 | set colorcolumn=0 | endif
        set foldcolumn=0
        set laststatus=0
        set noshowmode
        set scrolloff=8
        if core#Prose() | set spell
        else            | set nospell | endif
        call s:view()
      endfunction

    " .............................................................. Switch View

      " toggle full document highlight
      function! s:view()
        Trace ui:view()
        let l:col = virtcol('.')
        call theme#Theme()
        if core#Prose() | call theme#ToggleProof() | endif
        if b:view == 1
          call s:showInfo(1)
          Limelight!
          call theme#Contrast(0)
        else
          call s:showInfo(0)
          Limelight
          call theme#Contrast(1)
        endif
        execute 'normal! ' . l:col . '|'
      endfunction

      function! ui#ToggleProof()
        Trace ui#ToggleProof()
        let b:view = b:view == 0 ? 1 : 0
        call s:view()
      endfunction

      function! s:setView()
        Trace ui:setView()
        if g:view == 0 | call s:codeView()
        else           | call s:dfmView() | endif
      endfunction

      " toggle dfm view
      function! ui#SwitchView()
        Trace ui#SwitchView()
        let l:col = col('.')
        let g:view = g:view == 0 ? 1 : 0
        call s:setView()
        execute 'normal! ' . l:col . '|'
      endfunction

  " Screen focus _______________________________________________________________

    " ........................................................... Screen display

      " initial view
      function! ui#LiteType()
        Trace ui#LiteType()
        call theme#FontSize(core#Prose() ? 1 : 0)
        call theme#Palette()
        if ! exists('b:view') | let b:view = s:initial_view | endif
        call s:setView()
      endfunction

      " redraw
      function! ui#Retheme()
        Trace ui#Refresh()
        let lstatus     = &laststatus
        call s:setView()
        let &laststatus = lstatus
      endfunction

  " Context statusline _________________________________________________________

    " ........................................................ Statusline format

      function! ui#Detail()
        let l:prefix = g:detail == 0 ? info#Tag() : info#Atom()
        return l:prefix > '' ? l:prefix . '  ' . info#SpecialChar() : info#SpecialChar()
      endfunction

      " [path] .. filename | pos .. [details]
      function! s:statusline(proof)
        " Trace ui:statusline()  " tmi :-)
        try  " trap snippet insertion interruption
          let g:prose = 1
          if core#Prose() && a:proof == 0
            return info#Escape(info#Leader('') . '  %{info#UnModified(0)}%*')
          else
            let l:name     = '%{info#Name()}' . g:pad_inner
            if s:expanded == 0  " center dfm indicator / proofing statusline
              let l:leader = '%{info#Leader(info#Name())}'
            else
              let l:path   = '%{info#Path()}'
              let l:leader = '%{info#Leader(info#Path() . g:pad_outer . info#Name())}'
            endif
            let l:name     = '%1*' . l:name
            let l:info     = '%{info#UnModified(1)}' . g:pad_inner . ' ' . '%{info#PosWordsCol()}'  " utf-8 symbol occupies 2 chars (pad right 1 space)
            if s:expanded == 1
              let l:name   = '%2*' . l:path . '%1*' . g:pad_outer . l:name
              let l:info  .= g:pad_outer . '%2*%{ui#Detail()}'
            endif
            return info#Escape('%1*' . l:leader . l:name . l:info . '%1*')
          endif
        catch /.*/  " discard messages
        endtry
      endfunction

    " .......................................................... Show statusline

      function! s:showInfo(proof)
        Trace ui:showInfo()
        if s:show == 1
          " execute 'set statusline=%{s:statusline(' . a:proof . ')}'
          execute 'set statusline=' . s:statusline(a:proof)
          call theme#ShowStatusline()
        else
          call theme#ShowInfo()  " simply hide statusline content
        endif
      endfunction

      function! ui#RefreshInfo()
        Trace ui#RefreshInfo()
        call s:showInfo(b:view)
      endfunction

      function! ui#ToggleInfo(...)
        Trace ui#ToggleInfo()
        if a:0 && a:1 | return | endif  " prose insert mode is always dfm
        let l:col = col('.')
        let s:expanded = (s:expanded == 0 ? 1 : 0)
        " if core#Prose() | call ui#ToggleProof()  " toggle between writing and proofing modes
        " else            | call s:showInfo(b:view) | endif
        call s:showInfo(b:view)
        execute 'normal! ' . l:col . '|'
      endfunction

" ui.vim
