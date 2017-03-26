" sdothum - 2016 (c) wtfpl

" Startup
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Vim ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      set nocompatible                      " disable vi-compatibility

      augroup startup
        autocmd!
      augroup END

   " .................................................................. Keyboard

      " first, get <leader>
      source ~/.vim/config/keyboard.vim

    " ................................................................... System

      set lazyredraw                        " don't redraw while executing macros
      " set modelines=0                     " prevent modeline secrurity hole
      set modelines=1
      set mouse=a                           " enable mouse actions
      set shell=/bin/sh                     " required for plugin system call dependencies
      set title                             " change the terminal's title
      set ttyfast
      set timeout timeoutlen=1000 ttimeoutlen=100
      " set cryptmethod=blowfish            " encryption method

    " ..................................................................... Swap

      set nobackup
      set directory=~/tmp,/tmp              " keep swap files in one location
      set noswapfile                        " turn off swap files
      set nowritebackup

  " Reload settings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Config file

      " quickly edit/reload the vimrc file
      nmap <silent><leader>vi         :edit $MYVIMRC<CR>
      " must switch to .vimrc first for unknown reason.. (bug?)
      nmap <silent><leader><leader>vi :buffer .vimrc<CR>:autocmd!<CR>:source $MYVIMRC<CR>

      " load .vimrc after save
      autocmd startup BufWritePost $MYVIMRC nested source $MYVIMRC
      autocmd startup BufWritePost ~/.vim/config/* buffer $MYVIMRC | source $MYVIMRC
      autocmd startup BufWinEnter  *.vim           set filetype=vim

" startup.vim
