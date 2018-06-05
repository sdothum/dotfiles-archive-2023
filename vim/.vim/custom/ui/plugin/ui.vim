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

      autocmd ui InsertEnter * call ui#LineNr('i')
      autocmd ui InsertLeave * call ui#LineNr('n')

  " UI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Toggle statusline

      " toggle statusline details
      nmap <silent><F7>   :call ui#ToggleInfo()<CR>
      imap <silent><F7>   <C-o>:call ui#ToggleInfo()<CR>
      vmap <silent><F7>   <C-o>:call ui#ToggleInfo()<CR>

      " auto dfm writing
      autocmd ui InsertEnter * if core#Prose() | call ui#ToggleProof(0) | endif
      autocmd ui InsertLeave * if core#Prose() | call ui#ToggleProof(1) | endif

    " .............................................................. Switch mode

      nmap <silent><S-F7> :call ui#SwitchView()<CR>
      imap <silent><S-F7> <C-o>:call ui#SwitchView()<CR>
      vmap <silent><S-F7> <C-o>:call ui#SwitchView()<CR>

      " intial view mode: source code or prose
      autocmd ui BufEnter * call ui#LiteType()

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

      autocmd ui FocusGained * silent! call ui#Margin()

      let &cpo = s:save_cpo
      unlet s:save_cpo

" ui.vim
