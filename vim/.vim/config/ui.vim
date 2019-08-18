" sdothum - 2016 (c) wtfpl

" Themes
" ══════════════════════════════════════════════════════════════════════════════

  " The view ___________________________________________________________________

    " .................................................................... Setup

      let g:detail      = 0   " default expanded detail (0) tag (1) atom, see F7 map
      let g:active      = 0   " active window tag

      " Iosevka custom compiled, with nerd-fonts awesome patches, see make_install/iosevka
      let g:source_font = 'Iosevka\'
      let g:prose_font  = 'Iosevka-proof\'
      let g:font_type   = -1  " current font setting (0) source (1) prose
      let g:font_step   = empty(glob('~/.session/font++')) ? 1 : 2  " increase (point size) for prose

      augroup ui | autocmd! | augroup END

  " Display ____________________________________________________________________

    " ................................................................... Redraw

      nmap <silent><F9>      :call Retheme()<CR>
      imap <silent><F9> <C-o>:call Retheme()<CR>

      " window manager timing requires FocusLost trap with VimResized to consistently set margins
      autocmd ui VimEnter,VimResized,FocusLost * call Margins()
      autocmd ui CursorHold,FocusGained        * if g:lite_dfm_left_offset != Offset() | call Margins() | endif

    " ............................................................... Initialize

      " intial view mode: source code or prose, plugin windows inherit current theme (avoids thrashing)
      autocmd ui BufWinEnter * if ! PluginWindow() | call LiteType() | endif
      " show and fix line wrap highlighting on startup
      autocmd ui GuiEnter    * if g:wrap_highlighting && ! PluginWindow() | call Retheme() | endif

    " ................................................................. Messages

      " recover last error message
      nmap <leader>e :echo errmsg<CR>

      " clear messages after awhile to keep screen clean and distraction free!
      autocmd ui CursorHold * echo

  " Highlighting _______________________________________________________________

    " ...................................................... Syntax highlighting

      set omnifunc=syntaxcomplete#Complete
      syntax on  " turn on syntax highlighting

      " refresh highlighting on arm
      autocmd ui CursorHold * if ! Prose() && &filetype != '' | execute 'set filetype=' . &filetype | endif

    " ...................................................... White space markers

      set nolist  " display tabs and trailing spaces visually
      set listchars="tab:▸\<Space>"

      " set listchars+=trail:_
      set listchars+=trail:·
      set listchars+=nbsp:.
      set listchars+=extends:>
      set listchars+=precedes:<
      " set listchars+=eol:¬

    " ..................................................... Trailing white space

      nmap <silent><leader><Space> :ToggleWhiteSpace<CR>

  " UI _________________________________________________________________________

    " ........................................................ Toggle statusline

      " toggle statusline details
      nmap <silent><F7>        :ToggleInfo<CR>
      imap <silent><F7>   <C-o>:ToggleInfo Prose()<CR>

      " toggle tag, line details
      nmap <silent><C-F7>      :let g:detail = g:detail == 0 ? 1 : 0<CR>
      imap <silent><C-F7> <C-o>:let g:detail = g:detail == 0 ? 1 : 0<CR>

      " for active window highlighting
      autocmd ui BufWinEnter,WinEnter,TerminalOpen,VimEnter * let g:active = g:active + 1 | let w:tagged = g:active | SplitColors
      autocmd ui WinLeave                                   * SplitColors

    " ................................................................ View mode

      nmap <silent><C-S-F7>      :call ToggleProof()<CR>
      imap <silent><C-S-F7> <C-o>:call ToggleProof()<CR>

      if has('gui_running')
        autocmd ui InsertEnter * call ToggleProof() | SignifyDisable
        autocmd ui InsertLeave * call ToggleProof() | SignifyEnable
      endif

    " .............................................................. Switch mode

      nmap <silent><S-F7>      :SwitchView<CR>
      imap <silent><S-F7> <C-o>:SwitchView<CR>

    " ......................................................... Switch font size

      nmap <silent><S-F9>      :call Font(g:font_type == 1 ? 0 : 1)<CR>
      imap <silent><S-F9> <C-o>:call Font(g:font_type == 1 ? 0 : 1)<CR>

" ui.vim
