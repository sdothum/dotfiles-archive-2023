" sdothum - 2016 (c) wtfpl

" Themes
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      if exists("g:loaded_ui")
        finish
      endif
      let g:loaded_ui = 1
      let s:save_cpo = &cpo
      set cpo&vim

      augroup ui
        autocmd!
      augroup END

    " ......................................................... DFM colour masks

      autocmd ui InsertEnter * execute 'highlight LineNr guifg=' . g:dfm_linenr_ins
      autocmd ui InsertLeave * execute 'highlight LineNr guifg=' . g:dfm_linenr_cmd

  " UI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch mode

      nmap <silent><F7>   :call ui#SwitchView()<CR>
      imap <silent><F7>   <C-o>:call ui#SwitchView()<CR>
      vmap <silent><F7>   <C-o>:call ui#SwitchView()<CR>

      " intial view mode: source code or prose
      autocmd ui BufEnter * call ui#LiteType()

    " ........................................................ Toggle statusline

      " toggle lightline/default vim statusline
      nmap <silent><S-F7> :call ui#ToggleInfo()<CR>
      imap <silent><S-F7> <C-o>:call ui#ToggleInfo()<CR>
      vmap <silent><S-F7> <C-o>:call ui#ToggleInfo()<CR>

  " Display ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Switch colour

      nmap <silent><F8>   :call ui#LiteSwitch()<CR>
      imap <silent><F8>   <C-o>:call ui#LiteSwitch()<CR>
      vmap <silent><F8>   <C-o>:call ui#LiteSwitch()<CR>

    " ......................................................... Switch font size

      nmap <silent><S-F9> :call ui#FontSize(g:font_type == 1 ? 0 : 1)<CR>
      imap <silent><S-F9> <C-o>:call ui#FontSize(g:font_type == 1 ? 0 : 1)<CR>
      vmap <silent><S-F9> <C-o>:call ui#FontSize(g:font_type == 1 ? 0 : 1)<CR>

    " ................................................................... Redraw

      nmap <silent><F9>   :call ui#Retheme()<CR>
      imap <silent><F9>   <C-o>:call ui#Retheme()<CR>
      vmap <silent><F9>   <C-o>:call ui#Retheme()<CR>

      let &cpo = s:save_cpo
      unlet s:save_cpo

" ui.vim
