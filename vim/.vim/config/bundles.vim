" sdothum - 2016 (c) wtfpl

" Bundles
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Plugin manager ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      " autocompletion plugin dependent on filetype
      let ext = expand('%:e')

      filetype off                              " safe startup (vundle requirement)

      " see https://github.com/junegunn/vim-plug
      " :PlugInstall/PlugUpdate/PlugClean
      " :PlugUpgrade (vim-plug)
      call plug#begin('~/.vim/plugged')

  " Interface ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................... Layout

      " Plug 'junegunn/goyo.vim'                " distraction free mode
      " Plug 'altercation/vim-colors-solarized' breaks synIDattr with vim8
      Plug 'junegunn/limelight.vim'             " hyperfocus highlighting
      Plug 'bilalq/lite-dfm'                    " distraction free mode
      Plug 'lifepillar/vim-solarized8'          " colour theme

    " ..................................................................... Info

      " Plug 'metakirby5/codi.vim'              " async evaluator
      " Plug 'scrooloose/syntastic'             " lint
      " Plug 'reedes/vim-wordy'                 " word usage
      Plug 'itchyny/lightline.vim'              " statusline
      Plug 'majutsushi/tagbar'                  " ctags
      Plug 'bimlas/vim-eightheader'             " custom foldtext

    " ................................................................... Keymap

      " Plug 'kana/vim-arpeggio'                " key chords

    " ............................................................. Highlighting

      Plug 'kien/rainbow_parentheses.vim'       " parentheses highlighting
      Plug 'dag/vim-fish'                       " shell highlighting
      Plug 'nathanaelkane/vim-indent-guides'    " colourized indent columns
      Plug 'plasticboy/vim-markdown'            " markdown highlighting
      Plug 'slim-template/vim-slim'             " dsl highlighting

  " Buffers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Management

      " Plug 'kien/ctrlp.vim'                   " fuzzy finder
      " Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --key-bindings' } installed via pacman
      " Plug 'junegunn/fzf.vim' doesn't integrate well with gvim
      " Plug 'duff/vim-scratch'                 " scratch buffer
      Plug 'chrisbra/NrrwRgn'                   " visual block buffer

    " .......................................................... Version control

      " Plug 'tpope/vim-fugitive'               " github wrapper
      " Plug 'ludovicchabant/vim-lawrencium'    " mercurial wrapper

    " ................................................................ Hypertext

      " Plug 'cwoac/nvim'                       " notational velocity
      " Plug 'greyblake/vim-preview'            " markdown
      Plug 'vimwiki/vimwiki'                    " markdown wiki

  " Editing ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Navigation

      Plug 'kshenoy/vim-signature'              " toggle marks
      Plug 'justinmk/vim-sneak'                 " jump to location

    " .................................................................. History

      Plug 'sjl/gundo.vim'                      " undo
      Plug 'vim-scripts/YankRing.vim'           " paste (yank)

    " ............................................................... Formatting

      Plug 'scrooloose/nerdcommenter'           " toggle comment
      Plug 'junegunn/vim-easy-align'            " align text objects
      Plug 'reedes/vim-pencil'                  " dynamic paragraph formatting

    " ............................................................... Completion

      " Plug 'mattn/emmet-vim'                  " html
      Plug 'auto-pairs'                         " insert/delete pairs
      Plug 'Shougo/neosnippet'                  " snippets
      Plug 'tpope/vim-endwise'                  " add 'end' statement
      Plug 'reedes/vim-litecorrect'             " spelling
      if ext == 'wiki'
        Plug 'lifepillar/vim-mucomplete'        " tab completion
      else
        Plug 'maxboisvert/vim-simple-complete'  " enter completion
      endif
      Plug 'tpope/vim-surround'                 " pairwise c'hange, d'elete, y'ank

  " Plugins ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Configure

      call plug#end()

      filetype plugin on
      filetype indent on                        " required
      filetype on

      source ~/.vim/config/plugins.vim

" bundles.vim
