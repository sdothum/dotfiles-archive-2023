" sdothum - 2016 (c) wtfpl
" Editing
" ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

  " Formatting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Input settings

      set formatoptions=qn1j                " coding options
      " double spaces at the end of a wrapped line, becomes <br> by markdown
      set nojoinspaces                      " force single spacing after sentence punctuation!
      set textwidth=80                      " normally 78-80, see autocmd for mail

    " ...................................................... Trailing whitespace

      " strip all trailing whitespace in the current file
      " nmap <C-F8>                 :%s/\s\+$//<cr>:let @/=""<CR>
      nmap <leader><F8>             :%s/\s\+$//<cr>:let @/=""<CR>

    " ...................................................... Reformat paragraghs

      " select all
      nnoremap <A-End>              ggVG

      " use Q for formatting the current paragraph (or selection)
      nnoremap Q                    gqap
      vnoremap Q                    gq
      " reformat paragraph, reformat and go to next
      inoremap <S-F4>               <ESC>mZ{gq}`Z:silent delmark Z<CR>a
      nnoremap <S-F4>               {gq}j
      " add trailing space to paragragh lines
      vnoremap <S-F4>               V:s/\(.*[^ ]\)\s*$/\1 /<CR>:silent nohlsearch<CR>

      " ........................................................ Wiki code block

      " markdown wiki code blocks
      inoremap <F5>                 <C-o>V:call CodeBlock()<CR>
      nnoremap <F5>                 V:call CodeBlock()<CR>
      vmap <F5>                     :call CodeBlock()<CR>
      " insert strikeout
      imap <leader><Delete>         <<c-o>idel>
      imap <leader><Delete><Delete> </del>

  " Indenting ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................. Defaults

      set autoindent
      set copyindent                        " copy the previous indentation on autoindenting
      set expandtab                         " expand tabs into spaces, never use hard tabs!
      set shiftround                        " use multiple of shiftwidth when indenting with "<" and ">"
      set shiftwidth=2                      " number of spaces for unindenting
      set nosmartindent                     " smartindent on outdents hash comments to beginning of line
      set smarttab
      set softtabstop=2
      set tabstop=2                         " global tab width

      cabbrev spaces set expandtab
      cabbrev tabs set noexpandtab

    " .................................................. Shift text left / right

      " see Vertical Text Shifting functions.vim
      nnoremap <S-Left>   <<
      nnoremap <S-Right>  >>
      inoremap <S-Left>   <C-d>
      inoremap <S-Right>  <C-t>
      " preserve selection when indenting
      vnoremap <S-Right>  >gv
      vnoremap <S-Left>   <gv

    " ............................................................. Convert tabs

      :command! -range=% -nargs=0 Tab2Space execute '<line1>,<line2>s#^\t\+#\=repeat(" ", len(submatch(0))*' . &ts . ')'
      :command! -range=% -nargs=0 Space2Tab execute '<line1>,<line2>s#^\( \{'.&ts.'\}\)\+#\=repeat("\t", len(submatch(0))/' . &ts . ')'
      nmap <F3>           :retab<CR>
      nmap <silent><C-F3> :Space2Tab<CR>
      vmap <silent><C-F3> :Space2Tab<CR>
      nmap <silent><S-F3> :Tab2Space<CR>
      vmap <silent><S-F3> :Tab2Space<CR>

    " .................................................... Filetype tab settings

      autocmd Filetype conf    setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype fish    setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype haskell setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype lua     setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype python  setlocal nospell expandtab tabstop=4 shiftwidth=4 softtabstop=4
      autocmd Filetype ruby    setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype shell   setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype sh      setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype slim    setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype vim     setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      autocmd Filetype vim     setlocal nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2

  " Line manipulation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................. Insert line / paragraph

      " continue inserting in new line a la textmate command-enter
      " ctrl-enter only works with gvim due to terminal limitation :-(
      inoremap <C-CR>                  <C-o>o
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

      set pastetoggle=<F8>                  " prevent cascading paste insert
      autocmd InsertLeave * set nopaste     " disable paste mode when leaving Insert Mode

      " yank from the cursor to the end of the line, to be consistent with C and D.
      " see yankring for plugin equivalent
      nnoremap Y          y$
      " reselect/reyank text just pasted
      nnoremap <leader>v  gv
      nnoremap <leader>y  gvy
      vnoremap <leader>p  pgvy

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

  " Abbreviations ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Current Date

      inoremap <silent><leader><leader>d <C-r>=tolower(strftime("%A, %-d %B %Y"))<CR>
      nnoremap <silent><leader><leader>d "=tolower(strftime("%A, %-d %B %Y"))<CR>P<CR>

    " .................................................. Lorem ipsum placeholder

      " pencil disables abbreviations..

      iabbrev lorem.. 
        \Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
        \ do eiusmod tempor incididunt ut labore et dolore magna aliqua.
        \ Ut enim ad minim veniam, quis nostrud exercitation ullamco
        \ laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
        \ dolor in reprehenderit in voluptate velit esse cillum dolore eu
        \ fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
        \ proident, sunt in culpa qui officia deserunt mollit anim id est
        \ laborum.
        \<CR><CR><ESC><DEL>

    " ............................................................... Signatures

      iabbrev bye.. 
        \Bye!<CR>
        \Steven<ESC>

      iabbrev dad.. 
        \(L)<CR>
        \:D(A):D<ESC>

      iabbrev hope.. 
        \Hope this helps,<CR>
        \Steven<ESC>

      iabbrev kindest.. 
        \Kindest regards,<CR>
        \Steven<ESC>

      iabbrev regards.. 
        \Regards,<CR>
        \Steven<ESC>

      iabbrev take.. 
        \Take good care,<CR>
        \Steven<ESC>

      iabbrev thanks.. 
        \Thanks,<CR>
        \Steven<ESC>

    " ........................................................... Script headers

      iabbrev dash.. 
        \#!/usr/bin/dash<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=sh: #
        \<ESC>:set ft=sh<CR>gg<down><left>

      iabbrev sh.. 
        \#!/bin/sh<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=sh: #
        \<ESC>:set ft=sh<CR>gg<down><left>

      iabbrev fish.. 
        \#!/usr/bin/fish<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=fish: #
        \<ESC>:set ft=fish<CR>gg<down><left>

      iabbrev zsh.. 
        \#!/usr/bin/zsh<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=zsh: #
        \<ESC>:set ft=zsh<CR>gg<down><left>

      iabbrev hs.. 
        \#!/usr/bin/ghci<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=hs: #
        \<ESC>:set ft=hs<CR>gg<down><left>

      iabbrev rb.. 
        \#!/usr/bin/ruby<CR><BS>
        \# encoding: UTF-8<CR><BS><BS><CR>
        \require 'term/ansicolor'<CR>
        \class String<CR>
        \  include Term::ANSIColor<CR>
        \end<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=ruby: #
        \<ESC>:set ft=ruby<CR>gg<down><left>

     " ........................................................... Vim modifiers

      iabbrev conf## 
        \<ESC>Go
        \<CR>
        \# vim: set ft=conf: #
        \<ESC>:set ft=conf<CR>gg<down><down><left>

      nmap <S-F1> :set ft=conf<CR>

" editing.vim
