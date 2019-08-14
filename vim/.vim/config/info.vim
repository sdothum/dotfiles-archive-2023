" sdothum - 2016 (c) wtfpl

" Info
" ══════════════════════════════════════════════════════════════════════════════

  " StatusLine _________________________________________________________________

    " .................................................................... Setup

      if $DISPLAY > ''
        let g:modified_ind     = ''  " nerd-font utf-8 symbols, see ui.vim
        let g:unmodified_ind   = ''  " nerd-font utf-8 symbols, see ui.vim
        let g:unmodifiable_ind = ''  " nerd-font utf-8 symbols, see ui.vim
        let g:inactive_ind     = ''  " nerd-font utf-8 symbols, see ui.vim
      else
        let g:modified_ind     = '+'
        let g:unmodified_ind   = ' '
        let g:unmodifiable_ind = '-'
        let g:inactive_ind     = ' '
      endif

      let g:prose            = 0      " generic filetype, see theme.vim
      let g:column           = 0      " statusline column indicator, see theme.vim

      augroup info | autocmd! | augroup END

    " ..................................................... Statusline indicator

      " trigger autocmd to flash column position (does not work for BOF)
      nnoremap <silent><C-c> hl

      autocmd info CursorHold  * let g:column = 0
      autocmd info CursorMoved * let g:column = 1

    " ............................................................. Syntax group
    
      nnoremap <silent><F10> :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>

" info.vim
