" sdothum - 2016 (c) wtfpl

" Abbreviations
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Composition ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

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

  " Coding ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................... Script headers

      iabbrev dash.. 
        \<ESC>ggO
        \#!/usr/bin/dash<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=sh: #
        \<ESC>:set ft=sh<CR>gg<down><left>

      iabbrev sh.. 
        \<ESC>ggO
        \#!/bin/sh<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=sh: #
        \<ESC>:set ft=sh<CR>gg<down><left>

      iabbrev fish.. 
        \<ESC>ggO
        \#!/usr/bin/fish<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=fish: #
        \<ESC>:set ft=fish<CR>gg<down><left>

      iabbrev zsh.. 
        \<ESC>ggO
        \#!/usr/bin/zsh<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=zsh: #
        \<ESC>:set ft=zsh<CR>gg<down><left>

      iabbrev hs.. 
        \<ESC>ggO
        \#!/usr/bin/ghci<CR>
        \<ESC>Go
        \<CR>
        \# vim: set ft=hs: #
        \<ESC>:set ft=hs<CR>gg<down><left>

      iabbrev rb.. 
        \<ESC>ggO
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

      iabbrev conf.. 
        \<ESC>Go
        \<CR>
        \# vim: set ft=conf: #
        \<ESC>:set ft=conf<CR>gg<down><down><left>

" abbrev.vim
