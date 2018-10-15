" sdothum - 2016 (c) wtfpl

" User Interface
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let s:wikiinfo     = 1                " statusline (0) off (1) on
      let s:initial_view = 1                " prose (0) dfm (1) proof
      let s:info         = 0                " statusline (0) dfm (1) expanded

  "  Distraction free modes ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Code view

      " source code style
      function! ui#CodeView()
        Trace ui#CodeView()
        let g:view = 0
        " restore CursorLine syntax highlighting before applying themes
        " syntax enable
        if exists('g:loaded_limelight')
          Limelight!
        endif
        call theme#Theme()
        call theme#ShowStatusline()
        call theme#CodeView()
        set showmode
      endfunction

    " .................................................... Distraction free view

      " prose style
      function! ui#DfmView()
        Trace ui#DfmView()
        let g:view = 1
        " silent !tmux set status off
        " un/comment to have monochromatic cursor line (looses vimdiff highlighting)
        call theme#DfmView()
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
        Trace ui#ToggleProof()
        let l:col = virtcol('.')
        let b:proof = b:proof == 0 ? 1 : 0
        call theme#Theme()
        if core#Prose()
          call theme#ToggleProof()
        endif
        if b:proof == 1
          call ui#ShowInfo(1)
          Limelight!
        else
          call ui#ShowInfo(0)
          Limelight
        endif
        execute 'normal! ' . l:col . '|'
      endfunction

      function! ui#SetView()
        Trace ui#SetView()
        if g:view == 0
          call ui#CodeView()
        else
          call ui#DfmView()
        endif
      endfunction

      " toggle dfm view
      function! ui#SwitchView()
        Trace ui#SwitchView()
        let l:col = col('.')
        let g:view = g:view == 0 ? 1 : 0
        call ui#SetView()
        execute 'normal! ' . l:col . '|'
      endfunction

  " Screen focus ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Screen display

      " initial view
      function! ui#LiteType()
        Trace ui#LiteType()
        call theme#FontSize(core#Prose() ? 1 : 0)
        call theme#Palette()
        if !exists('b:proof')
          let b:proof = s:initial_view
        endif
        call ui#SetView()
      endfunction

      " redraw
      function! ui#Retheme()
        Trace ui#Refresh()
        let lstatus     = &laststatus
        call ui#SetView()
        let &laststatus = lstatus
      endfunction

  " Context statusline ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Statusline format

      function! ui#detail()
        return (g:detail == 0 ? info#Tag() : info#Atom()) . '  ' . info#SpecialChar()
      endfunction

      " (path) .. filename | pos .. (details)
      function! ui#WikiInfo(proof)
        " Trace ui#WikiInfo()
        try                                 " trap snippet insertion interruption
          let g:prose = 1
          if core#Prose() && a:proof == 0
            return info#Escape(info#Leader('') . '  %{info#UnModified(0)}%*')
          else
            let l:name     = '%{info#Name()}' . g:pad_inner
            " center dfm indicator / proofing statusline
            if s:info == 0
              let l:leader = '%{info#Leader(info#Name())}'
            else
              let l:path   = '%{info#Path()}'
              let l:leader = '%{info#Leader(info#Path() . g:pad_outer . info#Name())}'
            endif
            let l:name     = '%1*' . l:name
            let l:info     = '%{info#UnModified(1)}' . g:pad_inner . '%{info#PosWordsCol()}'
            if s:info == 1
              let l:name   = '%2*' . l:path . '%1*' . g:pad_outer . l:name
              let l:info  .= g:pad_outer . '%2*%{ui#detail()}'
            endif
            return info#Escape('%1*' . l:leader . l:name . l:info . '%1*')
          endif
        catch
        endtry
      endfunction

      function! ui#ShowInfo(proof)
        Trace ui#ShowInfo()
        if s:wikiinfo == 1
          " execute 'set statusline=%{ui#WikiInfo(' . a:proof . ')}'
          execute 'set statusline=' . ui#WikiInfo(a:proof)
          call theme#ShowStatusline()
        else
          " simply hide statusline content
          call theme#ShowInfo()
        endif
      endfunction

      function! ui#RefreshInfo()
        Trace ui#RefreshInfo()
        call ui#ShowInfo(b:proof)
      endfunction

    " ........................................................ Toggle statusline

      function! ui#ToggleInfo(...)
        Trace ui#ToggleInfo()
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
