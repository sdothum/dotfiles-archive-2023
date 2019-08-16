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

  " Folding ____________________________________________________________________

    " ............................................................. Fold methods

      set foldenable            " fold by default
      set foldlevelstart=10     " open most folds by default
      " set foldlevelstart=1
      set foldnestmax=10        " 10 nested fold max
      " set foldmethod=indent   " fold based on indent
      set foldmethod=syntax     " folding based on syntax

      let javaScript_fold=1     " JavaScript
      let perl_fold=1           " Perl
      let php_folding=1         " PHP
      let r_syntax_folding=1    " R
      let ruby_fold=1           " Ruby
      let sh_fold_enabled=1     " sh
      let vimsyn_folding='af'   " Vim script
      let xml_syntax_folding=1  " XML

      " " toggle fold tag / open all
      " noremap <leader>z         za
      " noremap <leader>Z         zA
      " noremap <leader><leader>z zR

    " ........................................................... Folding levels

      nmap <silent><leader>0 :set foldlevel=0<CR>
      nmap <silent><leader>1 :set foldlevel=1<CR>
      nmap <silent><leader>2 :set foldlevel=2<CR>
      nmap <silent><leader>3 :set foldlevel=3<CR>
      nmap <silent><leader>4 :set foldlevel=4<CR>
      nmap <silent><leader>5 :set foldlevel=5<CR>
      nmap <silent><leader>6 :set foldlevel=6<CR>
      nmap <silent><leader>7 :set foldlevel=7<CR>
      nmap <silent><leader>8 :set foldlevel=8<CR>
      nmap <silent><leader>9 :set foldlevel=9<CR>

" info.vim
