" sdothum - 2016 (c) wtfpl

" Themes
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The view ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      if exists("g:loaded_ui")
        finish
      endif
      let g:loaded_ui = 1
      let s:save_cpo = &cpo
      set cpo&vim

      let g:detail  = 0                     " at cursor (0) tag (1) atom
      let g:view    = 1                     " initial view mode (0) info (1) dfm
      let g:padding = '     '               " expanded statusline padding

      augroup ui
        autocmd!
      augroup END

  " UI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Toggle statusline

      " toggle statusline details
      nmap <silent><F7>   :call ui#ToggleInfo()<CR>
      imap <silent><F7>   <C-o>:call ui#ToggleInfo()<CR>
      vmap <silent><F7>   <C-o>:call ui#ToggleInfo()<CR>

      " toggle tag, line details
      nmap <silent><C-F7> :let g:detail = g:detail == 0 ? 1 : 0<CR>
      imap <silent><C-F7> <C-o>:let g:detail = g:detail == 0 ? 1 : 0<CR>
      vmap <silent><C-F7> <C-o>:let g:detail = g:detail == 0 ? 1 : 0<CR>

      " default dfm writing, InsertChange required for insert mode F7 toggle
      autocmd ui InsertEnter  * if core#Prose() | call ui#ToggleInfo() | execute 'SignifyDisable' | endif
      autocmd ui InsertChange * if core#Prose() | call ui#ToggleInfo() | execute 'SignifyDisable' | endif
      autocmd ui InsertLeave  * if core#Prose() | call ui#ToggleInfo(1) | execute 'SignifyEnable' | endif

    " .............................................................. Switch mode

      nmap <silent><S-F7> :call ui#SwitchView()<CR>
      imap <silent><S-F7> <C-o>:call ui#SwitchView()<CR>
      vmap <silent><S-F7> <C-o>:call ui#SwitchView()<CR>

      " intial view mode: source code or prose, plugin windows inherit current theme (avoids thrashing)
      autocmd ui BufEnter * if !core#PluginWindow() | call ui#LiteType() | endif

  " Display ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................... Redraw

      nmap <silent><F9>   :call ui#Retheme()<CR>
      imap <silent><F9>   <C-o>:call ui#Retheme()<CR>
      vmap <silent><F9>   <C-o>:call ui#Retheme()<CR>

      let &cpo = s:save_cpo
      unlet s:save_cpo

" ui.vim
