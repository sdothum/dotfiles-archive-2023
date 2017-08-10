" sdothum - 2016 (c) wtfpl

" Themes
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      augroup ui
        autocmd!
      augroup END

    " ......................................................... DFM colour masks

      autocmd ui InsertEnter * execute 'highlight LineNr guifg=' . g:dfm_linenr_ins
      autocmd ui InsertLeave * execute 'highlight LineNr guifg=' . g:dfm_linenr_cmd

    " Contrast ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Switch colour

      nmap <silent><F7>   :call ui#LiteSwitch()<CR>
      nmap <silent><C-F7> :call ui#LiteFix()<CR>

      autocmd ui BufEnter * call ui#LiteFix()

  " Font ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Switch font

      nmap <silent><S-F7> :call ui#FontSwitch()<CR>

      autocmd ui BufEnter * call ui#FontSize(Prose() ? +1 : -1)

  " Screen focus ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Screen display

      function! Refresh()
        if Prose()
          let lstatus     = &laststatus
          call ui#Theme()
          let &laststatus = lstatus
        else
          call ui#CodeView()
        endif
        call ui#Cursor()                       " restore cursor (fullscreen toggling reverts defaults)
      endfunction

      imap <silent><F9> <C-o>:call Refresh()<CR>
      nmap <silent><F9> :call Refresh()<CR>

      " intial view mode: source code or prose
      autocmd ui BufEnter * call ui#LiteType()
      autocmd ui VimEnter * call ui#LiteType()

  " Enhanced statusline ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Toggle statusline

      " toggle lightline/default vim statusline
      imap <silent><F8> <C-o>:call ui#ToggleInfo()<CR>
      nmap <silent><F8> :call ui#ToggleInfo()<CR>

" views.vim
