" sdothum - 2016 (c) wtfpl

" Themes
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " The look ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      let g:size = 0                        " font size

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

      " adjust font sizes for various gpu's/displays, liteDFM offsets to fit screens
      function! FontSize(size)
        if $DISPLAY > ''
          if g:size == a:size
            return
          endif
          let g:size = a:size
          let l:size = system('fontsize')
          let l:guif = substitute(&guifont, '\([0-9]*\)', '\1', '')

          if l:guif == (l:size + 1) || a:size < 0
            call ui#Fontspace(l:size, 0)
            " let g:lite_dfm_left_offset = 22
          else
            call ui#Fontspace(l:size + 1, 1)
            " let g:lite_dfm_left_offset = 18
          endif

          " fix statusline/commandline position (drawn outside window)
          sleep 10m                           " delay long enough for font refresh
          call RefreshGui()
        endif
      endfunction

      nmap <silent><S-F7> :call ui#FontSwitch()<CR>

      " also called from dmenu compose script
      autocmd ui BufEnter * call FontSize(Prose() ? +1 : -1)

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
