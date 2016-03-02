" sdothum - 2016 (c) wtfpl

" Editing
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Formatting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Input settings

      set formatoptions=qrn1j               " coding options
      let g:codeoptions = &formatoptions
      " double spaces at the end of a wrapped line, becomes <br> by markdown
      set nojoinspaces                      " force single spacing after sentence punctuation!
      set textwidth=80                      " normally 78-80, see autocmd for mail
      let g:linewidth = &textwidth          " see coding.vim, drawing.vim, statusline.vim

    " ...................................................... Trailing whitespace

      " strip all trailing whitespace in the current file
      " nmap <C-F2>                :%s/\s\+$//<CR>:let @/=""<CR>
      nmap <leader><Space><Delete> :%s/\s\+$//<CR>:let @/=""<CR>

    " ...................................................... Reformat paragraghs

      " select all
      nnoremap <A-End>   ggVG

      " use Q for formatting the current paragraph (or selection)
      nnoremap Q         gqap
      vnoremap Q         gq
      " reformat paragraph, reformat and go to next
      " inoremap <S-F4>  <ESC>mZ{gq}`Z:silent delmark Z<CR>a
      " nnoremap <S-F4>  {gq}j
      nnoremap <leader>[ {gq}j
      " add trailing space to paragragh lines
      " vnoremap <S-F4>            V:s/\(.*[^ ]\)\s*$/\1 /<CR>:silent nohlsearch<CR>
      vnoremap <leader>] V:s/\(.*[^ ]\)\s*$/\1 /<CR>:silent nohlsearch<CR>

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
      " nmap <F3>                        :retab<CR>
      " nmap <silent><C-F3>              :Space2Tab<CR>
      " vmap <silent><C-F3>              :Space2Tab<CR>
      " nmap <silent><S-F3>              :Tab2Space<CR>
      " vmap <silent><S-F3>              :Tab2Space<CR>
      nmap <leader><tab>                 :retab<CR>
      nmap <silent><leader><leader><tab> :Space2Tab<CR>
      vmap <silent><leader><leader><tab> :Space2Tab<CR>
      nmap <silent><leader><tab><tab>    :Tab2Space<CR>
      vmap <silent><leader><tab><tab>    :Tab2Space<CR>

  " Line manipulation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................. Insert line / paragraph

      " continue inserting in new line a la textmate command-enter
      " ctrl-enter only works with gvim due to terminal limitation :-(
      " see InsertWrap coding.vim
      " inoremap <C-CR>                <C-o>o
      " similarly, open curly braces and continue inserting in indented body
      inoremap <S-CR>                  <CR><C-o>O<Tab>

      " break line (in .wiki)
      nnoremap <silent><leader><Enter> :set paste<CR>i<CR><ESC>:set nopaste<CR>i

      " insert blank line above/below
      nnoremap <silent><leader><Up>    :set paste<CR>m`O<Esc>``:set nopaste<CR>
      nnoremap <silent><leader><Down>  :set paste<CR>m`o<Esc>``:set nopaste<CR>

    " .............................................................. Delete line

      " delete blank line above/below
      nnoremap <silent><C-Up>          m`:silent -g/\m^\s*$/d<CR>``:nohlsearch<CR>
      nnoremap <silent><C-Down>        m`:silent +g/\m^\s*$/d<CR>``:nohlsearch<CR>

  " Copying and pasting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Shortcuts

      " prevent cascading paste insert
      set pastetoggle=<F2>
      autocmd InsertLeave * set nopaste     " disable paste mode when leaving Insert Mode

      " yank from the cursor to the end of the line, to be consistent with C and D.
      " see yankring for plugin equivalent
      nnoremap Y          y$
      " reselect/reyank text just pasted
      nnoremap <leader>V  gv
      nnoremap <leader>Y  gvy
      vnoremap <leader>P  pgvy

    " ...................................................... Sentence operations

      " use "as" suffix for outer sentence
      " change sentence
      nnoremap <leader>cc cis
      " cut sentence
      nnoremap <leader>dd dis
      " yank sentence
      nnoremap <leader>yy yis

    " .................................................... Clipboard cut / paste

      " visual mode yank/cut clipboard actions
      " "+Y yank line to clipboard
      vnoremap <C-c>      "+y
      vnoremap ys         "+y
      vnoremap yS         "+Y
      " "+D cut line to clipboard
      vnoremap <C-d>      "+d
      vnoremap yd         "+d
      vnoremap yD         "+D

      " normal/insert mode paste actions
      " "+P paste before from clipboard
      inoremap <C-v>      <C-o>"+p
      nnoremap <C-v>      "+p
      inoremap <C-S-v>    <ESC>"+pi
      nnoremap <C-S-v>    "+P

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
