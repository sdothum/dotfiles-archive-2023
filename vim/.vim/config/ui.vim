" sdothum - 2016 (c) wtfpl

" Themes
" ══════════════════════════════════════════════════════════════════════════════

  " The view ___________________________________________________________________

    " .................................................................... Setup

      let g:detail = 0  " at cursor (0) tag (1) atom
      let g:active = 0  " active window tag

      augroup ui | autocmd! | augroup END

  " Display ____________________________________________________________________

    " ................................................................... Redraw

      nmap <silent><F9> :call Retheme()<CR>
      imap <silent><F9> <C-o>:call Retheme()<CR>

    " ............................................................... Initialize

      " intial view mode: source code or prose, plugin windows inherit current theme (avoids thrashing)
      autocmd ui BufWinEnter * if ! PluginWindow() | call LiteType() | endif
      " show and fix line wrap highlighting on startup
      autocmd ui GuiEnter    * if g:wrap_highlighting && ! PluginWindow() | call Retheme() | endif

  " UI _________________________________________________________________________

    " ........................................................ Toggle statusline

      " toggle statusline details
      nmap <silent><F7>   :ToggleInfo<CR>
      imap <silent><F7>   <C-o>:ToggleInfo Prose()<CR>

      " toggle tag, line details
      nmap <silent><C-F7> :let g:detail = g:detail == 0 ? 1 : 0<CR>
      imap <silent><C-F7> <C-o>:let g:detail = g:detail == 0 ? 1 : 0<CR>

      " for active window highlighting
      autocmd ui BufWinEnter,WinEnter,TerminalOpen,VimEnter * let g:active = g:active + 1 | let w:tagged = g:active | SplitColors
      autocmd ui WinLeave                                   * SplitColors

    " ................................................................ View mode

      nmap <silent><C-S-F7> :call ToggleProof()<CR>
      imap <silent><C-S-F7> <C-o>:call ToggleProof()<CR>

      if has('gui_running')
        autocmd ui InsertEnter * call ToggleProof() | SignifyDisable
        autocmd ui InsertLeave * call ToggleProof() | SignifyEnable
      endif

    " .............................................................. Switch mode

      nmap <silent><S-F7> :SwitchView<CR>
      imap <silent><S-F7> <C-o>:SwitchView<CR>

" ui.vim
