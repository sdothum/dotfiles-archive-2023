" sdothum - 2016 (c) wtfpl

" Editing
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Formatting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

      augroup editing
        autocmd!
      augroup END

    " ........................................................... Input settings

      set formatoptions=qrn1j               " coding options
      let g:codeoptions = &formatoptions
      " double spaces at the end of a wrapped line, becomes <br> by markdown
      set nojoinspaces                      " force single spacing after sentence punctuation!
      set textwidth=80                      " normally 78-80, see autocmd for mail
      let g:linewidth = &textwidth          " see coding.vim, drawing.vim, statusline.vim

    " ...................................................... Reformat paragraghs

      " select all
      nnoremap <A-End>      ggVG
      " retain cursor position for insert mode reformatting
      inoremap <silent><F4> <Esc>lmZ{jv}kJvgq`Z:delmarks Z<CR>i
      " otherwise advance cursor to next paragraph
      nnoremap <F4>         {jv}kJvgq}}{j
      vnoremap <F4>         Jvgqj

    " .................................................. Quote enclose selection

      " extend enclosing %V 1 char right to enclose last character of block
      vnoremap '            :s/\%V\(.*\%V.\)/'\1'/<CR>:noh<CR>`>l
      vnoremap "            :s/\%V\(.*\%V.\)/"\1"/<CR>:noh<CR>`>l

  " Indenting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................. Defaults

      set autoindent
      set copyindent                        " copy the previous indentation on autoindenting
      set expandtab                         " expand tabs into spaces, never use hard tabs!
      set shiftround                        " use multiple of shiftwidth when indenting with "<>"
      set shiftwidth=2                      " number of spaces for unindenting
      set nosmartindent                     " smartindent hash comments to beginning of line
      set smarttab
      set softtabstop=2
      set tabstop=2                         " global tab width

      cabbrev spaces set expandtab
      cabbrev tabs set noexpandtab

    " .................................................. Shift text left / right

      " see Vertical Text Shifting functions.vim
      nnoremap <S-Left>  <<
      nnoremap <S-Right> >>
      inoremap <S-Left>  <C-d>
      inoremap <S-Right> <C-t>
      " preserve selection when indenting
      vnoremap <S-Right> >gv
      vnoremap <S-Left>  <gv

    " ............................................................. Convert tabs

      :command! -range=% -nargs=0 Tab2Space execute '<line1>,<line2>s#^\t\+#\=repeat(" ", len(submatch(0))*' . &ts . ')'
      :command! -range=% -nargs=0 Space2Tab execute '<line1>,<line2>s#^\( \{'.&ts.'\}\)\+#\=repeat("\t", len(submatch(0))/' . &ts . ')'

      nmap <silent><leader><tab>         :silent retab<CR>
      nmap <silent><leader><leader><tab> :silent Space2Tab<CR>
      vmap <silent><leader><leader><tab> :silent Space2Tab<CR>
      nmap <silent><leader><tab><tab>    :silent Tab2Space<CR>
      vmap <silent><leader><tab><tab>    :silent Tab2Space<CR>

  " Line manipulation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................. Insert line / paragraph

      " continue inserting in new line a la textmate command-enter
      " ctrl-enter only works with gvim due to terminal limitation :-(
      " see InsertWrap coding.vim
      " inoremap <C-CR>                <C-o>o
      " similarly, open curly braces and continue inserting in indented body
      inoremap <S-CR>                  <CR><C-o>O<Tab>

      " break line (in .wiki)
      nnoremap <silent><leader><Enter> :silent set paste<CR>i<CR><ESC>:silent set nopaste<CR>i

      " insert blank line above/below
      nnoremap <silent><leader><Up>    :silent set paste<CR>m`O<Esc>``:silent set nopaste<CR>
      nnoremap <silent><leader><Down>  :silent set paste<CR>m`o<Esc>``:silent set nopaste<CR>

    " .............................................................. Delete line

      " delete blank line above/below
      nnoremap <silent><C-Up>          m`:silent -g/\m^\s*$/d<CR>``:silent nohlsearch<CR>
      nnoremap <silent><C-Down>        m`:silent +g/\m^\s*$/d<CR>``:silent nohlsearch<CR>

  " Copying and pasting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Shortcuts

      " prevent cascading paste insert
      set pastetoggle=<F3>
      autocmd editing InsertLeave * set nopaste     " disable paste mode when leaving Insert Mode

      " yank from the cursor to the end of the line, to be consistent with C and D.
      " see yankring for plugin equivalent
      nnoremap Y          y$
      " reselect/reyank text just pasted
      nnoremap <leader>v  gv
      nnoremap <leader>V  gvy
      map <leader>p       pgvy

      " highlight last inserted text
      nnoremap <leader>i  `[v`]

    " ...................................................... Sentence operations

      " use "as" suffix for outer sentence
      " change sentence
      nnoremap <leader>cc cis
      " cut sentence
      nnoremap <leader>dd dis
      " yank sentence
      nnoremap <leader>yy yis

    " .................................................... Clipboard cut / paste

      nnoremap <C-a>      "ggVG+y

      " visual mode yank/cut clipboard actions
      " "+Y yank line to clipboard
      vnoremap <C-c>      "+y
      vnoremap ys         "+y
      vnoremap yS         "+Y
      " "+D cut line to clipboard
      vnoremap <C-d>      "+d
      vnoremap yd         "+d
      vnoremap yD         "+D

      " " normal/insert mode paste actions
      " " "+P pads space after insert
      " " note: to enter visual block mode type v<C-v>
      " inoremap <C-v>    <ESC>"+pli
      " nnoremap <C-v>    h"+pl
      imap <F2>           <ESC>"+pli
      nmap <F2>           h"+pl

  " Spelling ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Spell check

      set dictionary=/usr/share/dict/words
      set complete+=k                       " <C-p> to complete list word
      set keywordprg=dict
      set nospell                           " spell checking off by default for code
      " set thesaurus=/usr/share/dict/thesaurus
      " set complete+=s                     " disabled, selection list too long

      highlight SpellBad guisp=red gui=undercurl,bold guifg=brown
      highlight SpellCap guisp=red gui=undercurl,bold guifg=black
      highlight SpellRare guisp=red gui=undercurl,bold guifg=blue
      highlight SpellLocal guisp=red gui=undercurl,bold guifg=green

" editing.vim
