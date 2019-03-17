" sdothum - 2016 (c) wtfpl

" Startup
" ══════════════════════════════════════════════════════════════════════════════

  " Vim ________________________________________________________________________

    " .................................................................... Setup

      set nocompatible                        " disable vi-compatibility
      let g:gui = has('gui_running') ? 1 : 0  " gvim or console

    " ................................................................... System

      set lazyredraw              " don't redraw while executing macros
      " set modelines=0           " prevent modeline secrurity hole
      set modelines=1
      set mouse=a                 " enable mouse actions
      set shell=/bin/sh           " required for plugin system call dependencies
      set title                   " change the terminal's title
      set ttyfast
      set timeout timeoutlen=1000 ttimeoutlen=100
      " set cryptmethod=blowfish  " encryption method

    " ..................................................................... Swap

      set nobackup
      set directory=~/tmp,/tmp    " keep swap files in one location
      set noswapfile              " turn off swap files
      set nowritebackup

" startup.vim
