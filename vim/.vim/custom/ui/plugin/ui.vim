" sdothum - 2016 (c) wtfpl

" Themes
" ══════════════════════════════════════════════════════════════════════════════

  " The view ___________________________________________________________________

    " .................................................................... Setup

      if exists("g:loaded_ui") | finish | endif
      let g:loaded_ui = 1
      let s:save_cpo  = &cpo
      set cpo&vim

      let g:detail    = 0       " at cursor (0) tag (1) atom
      let g:view      = 1       " initial view mode (0) info (1) dfm
      let g:pad_inner = '    '  " statusline padding
      let g:pad_outer = '   '   " expanded statusline padding

      augroup ui | autocmd! | augroup END

  " Display ____________________________________________________________________

    " ................................................................... Redraw

      command! Retheme call ui#Retheme()

      nmap <silent><F9> :Retheme<CR>
      imap <silent><F9> <C-o>:Retheme<CR>
      vmap <silent><F9> :<C-u>Retheme<CR>

    " ............................................................... Initialize

      " intial view mode: source code or prose, plugin windows inherit current theme (avoids thrashing)
      autocmd ui BufEnter,BufWinEnter * if ! core#PluginWindow() | call ui#LiteType() | endif
      " show and fix line wrap highlighting on startup
      autocmd ui GuiEnter             * if ! core#PluginWindow() | call ui#LiteType() | call ui#Retheme() | endif

  " UI _________________________________________________________________________

    " ........................................................ Toggle statusline

      " toggle statusline details
      nmap <silent><F7>   :call ui#ToggleInfo()<CR>
      imap <silent><F7>   <C-o>:call ui#ToggleInfo(core#Prose())<CR>
      vmap <silent><F7>   :<C-u>call ui#ToggleInfo(core#Prose())<CR>

      " toggle tag, line details
      nmap <silent><C-F7> :let g:detail = g:detail == 0 ? 1 : 0<CR>
      imap <silent><C-F7> <C-o>:let g:detail = g:detail == 0 ? 1 : 0<CR>
      vmap <silent><C-F7> :<C-u>let g:detail = g:detail == 0 ? 1 : 0<CR>

      if g:gui
        autocmd ui InsertEnter * call ui#ToggleProof() | SignifyDisable
        autocmd ui InsertLeave * call ui#ToggleProof() | SignifyEnable
      endif

    " .............................................................. Switch mode

      nmap <silent><S-F7> :call ui#SwitchView()<CR>
      imap <silent><S-F7> <C-o>:call ui#SwitchView()<CR>
      vmap <silent><S-F7> :<C-u>call ui#SwitchView()<CR>

      let &cpo = s:save_cpo
      unlet s:save_cpo

" ui.vim
