" sdothum - 2016 (c) wtfpl

" Plugins
" ══════════════════════════════════════════════════════════════════════════════

" Plugin manager _______________________________________________________________

" ................................................................... Initialize
filetype off  " safe startup (vundle requirement)

" see https://github.com/junegunn/vim-plug
" :PlugInstall/PlugUpdate/PlugClean
" :PlugUpgrade (vim-plug)
call plug#begin('~/.vim/plugged')

" ....................................................................... System
Plug 'Shougo/vimproc.vim', { 'do' : 'make' }
" Plug 'tpope/vim-scriptease'          " debugger
" Plug 'tpope/vim-dispatch'            " launch async shell command

" User Interface _______________________________________________________________

" ....................................................................... Layout
Plug 'junegunn/limelight.vim'          " hyperfocus highlighting
Plug 'bilalq/lite-dfm'                 " distraction free mode
Plug 'TaDaa/vimade'                    " hyperfocus highlghting for buffers
Plug 'sdothum/vim-duochrome'           " distraction free monochromatic content driven layout

" ......................................................................... Info
" Plug 'metakirby5/codi.vim'           " async evaluator
" Plug 'tpope/vim-scriptease'          " meta plugin
" Plug 'reedes/vim-wordy'              " word usage
Plug 'majutsushi/tagbar'               " ctags
Plug 'lvht/tagbar-markdown'            " markdown for tagbar
Plug 'bimlas/vim-eightheader'          " custom foldtext
Plug 'junegunn/vim-peekaboo'           " registers

" ....................................................................... Keymap
" Plug 'kana/vim-arpeggio'             " key chords
" Plug 'Jorengarenar/pseudoClip'       " clipboard registers
Plug 'tpope/vim-repeat'                " dot plugin
Plug 'tpope/vim-rsi'                   " readline keybindings

" ................................................................. Highlighting
" Plug 'JuliaEditorSupport/julia-vim'  " julia programming language
" Plug 'markonm/traces.vim'            " ex pattern/range highlghting
" Plug 'romainl/vim-cool'              " auto clear search highlighting
Plug 'itchyny/vim-cursorword'          " word highlighting
Plug 'nathanaelkane/vim-indent-guides' " colourized indent columns
Plug 'plasticboy/vim-markdown'         " concealed markdown
Plug 'sheerun/vim-polyglot'            " multilingual highlighting
Plug 'machakann/vim-highlightedyank'   " yank highlghting

" Buffers ______________________________________________________________________

" ................................................................... Management
" Plug 'duff/vim-scratch'              " scratch buffer
" Plug 'simeji/winresizer'             " resize windows
Plug 'chrisbra/NrrwRgn'                " visual block buffer

" .............................................................. Version control
" Plug 'tpope/vim-fugitive'            " github wrapper
" Plug markwoodhall/vim-mercurial      " mercurial wrapper
Plug 'mhinz/vim-signify'               " vcs diff

" .................................................................... Hypertext
" Plug 'dyng/ctrlsf.vim'               " ctrl-shift-f clone
" Plug 'lervag/wiki'                   " vimwiki clone
Plug 'junegunn/fzf'                    " fuzzy finder (separate plug install to avoid update errors)
Plug 'junegunn/fzf.vim'                " fuzzy finder
Plug 'sdothum/notational-fzf-vim-duochrome'  " fzf content search (patched)

" ...................................................................... History
Plug 'simnalamburt/vim-mundo'          " undo
Plug 'vim-scripts/YankRing.vim'        " paste (yank)

" ....................................................................... Linter
Plug 'w0rp/ale'                        " asynchronous lint engine

" Editing ______________________________________________________________________

" ................................................................... Navigation
" Plug 'wellle/targets.vim'            " text objects
Plug 'Konfekt/FastFold'                " update folds
Plug 'masukomi/vim-markdown-folding'   " markdown foldexpr
Plug 'kshenoy/vim-signature'           " toggle marks
Plug 'justinmk/vim-sneak'              " jump to location

" ................................................................. Text objects
" Plug 'reedes/vim-textobj-quote'      " typographic characters
Plug 'tpope/vim-surround'              " pairwise c'hange, d'elete, y'ank
Plug 'glts/vim-textobj-comment'        " 'c' select comment
Plug 'kana/vim-textobj-fold'           " 'z' select fold
Plug 'kana/vim-textobj-function'       " 'f' select function
Plug 'kana/vim-textobj-indent'         " 'i' select with same indent level
Plug 'kana/vim-textobj-lastpat'        " '/' select last search pattern
Plug 'kana/vim-textobj-user'           " dependency for vim-textobj's

" ................................................,................... Formatting
Plug 'scrooloose/nerdcommenter'        " toggle comment
Plug 'junegunn/vim-easy-align'         " align text objects
Plug 'reedes/vim-pencil'               " dynamic paragraph formatting
Plug 'sdothum/vim-heading'             " comment heading formatter

" ................................................................... Completion
" Plug 'mattn/emmet-vim'               " html
Plug 'jiangmiao/auto-pairs'            " insert/delete pairs
Plug 'Shougo/neocomplete.vim'          " required by snippets
Plug 'Shougo/neosnippet.vim'           " snippets
Plug 'tpope/vim-endwise'               " add 'end' statement
Plug 'reedes/vim-litecorrect'          " autocorrections

" Plugin completion ____________________________________________________________

" .................................................................... Configure
call plug#end()

filetype plugin on
filetype indent on  " required
filetype on

" plugin.vim
