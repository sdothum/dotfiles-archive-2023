" sdothum - 2016 (c) wtfpl

" Initialization
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Vim ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Compatibility

      set nocompatible                      " disable vi-compatibility

    " ................................................................... Leader

      let mapleader   = "\<Space>"          " remap <leader> a la spacemacs
      let g:mapleader = "\<Space>"
      " let mapleader   = "\<BS>"           " use right thumb on planck keyboard
      " let g:mapleader = "\<BS>"           " for better <space> responsiveness

      " non-latent space insertion (for lining up text, conflicting leader sequences, etc.)
      " inoremap <C-Space> <Space>

    " .............................................................. Config file

      " quickly edit/reload the vimrc file
      nmap <silent><leader>vim         :edit $MYVIMRC<CR>
      " must switch to .vimrc first for unknown reason.. (bug?)
      nmap <silent><leader><leader>vim :buffer .vimrc<CR>:autocmd!<CR>:source $MYVIMRC<CR>

      " load .vimrc after save
      augroup config
        autocmd BufWritePost $MYVIMRC nested source $MYVIMRC
        autocmd BufWritePost ~/.vim/config/* buffer $MYVIMRC | source $MYVIMRC
        autocmd BufWinEnter  *.vim           set filetype=vim
      augroup END

" init.vim
