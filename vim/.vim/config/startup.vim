" sdothum - 2016 (c) wtfpl

" Startup
" ══════════════════════════════════════════════════════════════════════════════

" Vim __________________________________________________________________________

" ........................................................................ Setup
set nocompatible                        " disable vi-compatibility

" ....................................................................... System
set lazyredraw              " don't redraw while executing macros
" set modelines=0           " prevent modeline secrurity hole
set modelines=1
set mouse=a                 " enable mouse actions
set shell=/bin/sh           " required for plugin system call dependencies
set title                   " change the terminal's title
set ttyfast
set timeout timeoutlen=1000 ttimeoutlen=100
" set cryptmethod=blowfish  " encryption method

" ......................................................................... Swap
set nobackup
set directory=~/tmp,/tmp    " keep swap files in one location
set noswapfile              " turn off swap files

"  ................................................................ Undo history

" keep persistent undo history across sessions, by storing in file
silent !mkdir ~/.vim/backups 2>/dev/null
set history=1000            " store lots of :cmdline history
set undodir=~/.vim/backups
set undofile
set undolevels=1000         " maximum number of changes that can be undone
set undoreload=10000        " maximum number lines to save for undo
set nowritebackup

" .................................................................. Spell check
set dictionary=/usr/share/dict/words
set complete+=k             " <C-p> to complete list word
set keywordprg=dict
set nospell                 " spell checking off by default for code
" " set thesaurus=/usr/share/dict/thesaurus
" set complete+=s           " disabled, selection list too long

" ....................................................................... Screen
set gcr=a:blinkon0          " disable cursor blink
set mousehide               " hide mouse when typing
set t_Co=256                " 256 color support
set viewoptions=folds,options,cursor,unix,slash
set virtualedit=block       " allow virtual editing in Visual block mode
" set virtualedit=onemore   " allow for cursor beyond last character
set winminheight=0          " windows can be 0 line high
set wrap                    " wrap lines for viewing

" ....................................................................... Alerts
set noerrorbells            " don't beep
set shortmess+=filmnrxoOtT  " abbrev. of messages (avoids "hit enter")
set visualbell              " no sounds

" startup.vim
