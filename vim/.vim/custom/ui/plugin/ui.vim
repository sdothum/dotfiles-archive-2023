" sdothum - 2016 (c) wtfpl

" Themes
" ══════════════════════════════════════════════════════════════════════════════

  " The view ___________________________________________________________________

    " .................................................................... Setup

      if exists("g:loaded_ui") | finish | endif
      let g:loaded_ui = 1
      let s:save_cpo  = &cpo
      set cpo&vim

      let g:detail = 0  " at cursor (0) tag (1) atom
      let g:view   = 1  " initial view mode (0) info (1) dfm
      let g:active = 0  " active window tag

      augroup ui | autocmd! | augroup END

  " Display ____________________________________________________________________

    " ................................................................... Redraw

      command! Retheme call ui#Retheme()

      nmap <silent><F9> :Retheme<CR>
      imap <silent><F9> <C-o>:Retheme<CR>

    " ............................................................... Initialize

      " intial view mode: source code or prose, plugin windows inherit current theme (avoids thrashing)
      autocmd ui BufWinEnter * if ! core#PluginWindow() | call ui#LiteType() | endif
      " show and fix line wrap highlighting on startup
      autocmd ui GuiEnter    * if g:wrap_highlighting && ! core#PluginWindow() | call ui#Retheme() | endif

  " UI _________________________________________________________________________

    " ........................................................ Toggle statusline

      " toggle statusline details
      nmap <silent><F7>   :call ui#ToggleInfo()<CR>
      imap <silent><F7>   <C-o>:call ui#ToggleInfo(core#Prose())<CR>

      " toggle tag, line details
      nmap <silent><C-F7> :let g:detail = g:detail == 0 ? 1 : 0<CR>
      imap <silent><C-F7> <C-o>:let g:detail = g:detail == 0 ? 1 : 0<CR>

      " for active window highlighting
      autocmd ui WinEnter,TerminalOpen,VimEnter * let g:active = g:active + 1 | let w:tagged = g:active

    " ................................................................ View mode

      nmap <silent><C-S-F7> :call ui#ToggleProof()<CR>
      imap <silent><C-S-F7> <C-o>:call ui#ToggleProof()<CR>

      if has('gui_running')
        autocmd ui InsertEnter * call ui#ToggleProof() | SignifyDisable
        autocmd ui InsertLeave * call ui#ToggleProof() | SignifyEnable
      endif

    " .............................................................. Switch mode

      nmap <silent><S-F7> :call ui#SwitchView()<CR>
      imap <silent><S-F7> <C-o>:call ui#SwitchView()<CR>

      let &cpo = s:save_cpo
      unlet s:save_cpo

" ui.vim
