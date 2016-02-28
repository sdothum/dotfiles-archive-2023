" sdothum - 2016 (c) wtfpl

" Files
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " File handling ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Filetype settings

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

      nmap <leader><leader>t :setfiletype<Space>

    " ............................................................... Modifiable

      let g:goyotypes = 'vimwiki\|mail'

      " [regex name, filetype, modifiable, wordcount] rule tuple
      " modifiable (0) nomodifiable (1) modifiable
      " wordcount (0) no word count (1) statusline with wordcount
      " note "/name" to represent "^name"
      let s:nametypes =
        \[
        \  ['conf$', 'conf', 1, 0]
        \, ['config$', 'conf', 1, 0]
        \, ['eml$', 'mail', 1, 1]
        \, ['error$', 'log', 0, 0]
        \, ['log$', 'log', 0, 0]
        \, ['rc$', 'rc', 1, 0]
        \, ['txt$', 'text', 0, 1]
        \, ['wiki$', 'vimwiki', 1, 1]
        \, ['\(^\|/\)readme', 'text', 0, 1]
        \]

      " [regex fileinfo, filetype, modifiable, readonly] rule tuple
      " modifiable (0) nomodifiable (1) modifiable
      " readonly (0) not set (1) set
      let s:contenttypes =
        \[
        \  ['binary', 'binary', 0, 1]
        \, ['no read permission', 'binary', 0, 1]
        \, ['text', 'text', 1, 0]
        \]

      " set buffer attributes
      function! CheckFiletype()
        let b:prose = 0
        " by name structure
        for [name, filetype, modifiable, wordcount] in s:nametypes
          " known filetypes can be processed by filename e.g. "mkd" readme files
          if expand('%') =~ name
            " assign unknown filetype
            let &filetype = (&filetype == '' ? filetype : &filetype)
            let &modifiable = modifiable
            let b:prose = wordcount
            break
          endif
        endfor
        " by file content if not autodetected
        if &filetype == ''
          for [content, filetype, modifiable, readonly] in s:contenttypes
            if system('file -i ' . expand('%') . '|cut -d: -f2') =~ content
              let &filetype = filetype
              let &modifiable = modifiable
              let &readonly = readonly
            endif
          endfor
        endif
      endfunction

      " toggle modifiable attribute
      " nmap <silent><C-F1>  :let &modifiable = (&modifiable == 0 ? 1 : 0)<CR>
      nmap <silent><leader>- :let &modifiable = (&modifiable == 0 ? 1 : 0)<CR>

      " check filetype on open
      autocmd BufNewFile,BufRead * call CheckFiletype()
      autocmd BufWinEnter        *.txt,*.txt.gz if &filetype == 'help' | set nomodifiable | let b:prose = 1 | endif

  " Documents ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Wikis

      function! Wiki(index)
        " open vimwiki index file
        execute 'normal ' . a:index . g:mapleader . "ww"
        " vimenter bypasses other autocmds and filetype assignments
        " producing an unitialized non-lightline (raw) statusline !!??
        call CheckFiletype()
      endfunction

      function! OpenWikis()
        " new 'maxhi' option is still a bit buggy
        redir! >/tmp/vim.log
        " skip index 1 "journal" wiki
        for index in reverse(range(2, len(g:vimwiki_list)))
          call Wiki(index)
        endfor
        " reset cursorline (for last open for some reason..)
        if &filetype =~ g:goyotypes
          call HiLite()
        endif
        redir END
      endfunction

      " open vimwiki ()
      autocmd VimEnter * if argv (0) == 'thedarnedestthing' | bdelete | call OpenWikis() | endif
      autocmd VimEnter * if &filetype =~ g:goyotypes | call ToggleGoyo(0) | endif

    " ................................................................... E-mail

      function! ComposeMail()
        " set default cursorline highlight
        call ProseView()
        " email has blank lines inserted externally (via sed) for replys to
        " avoid the previously messy and unpredictable editing mode vim commands
        " see bin/dcompose
        execute 'normal 4G'
        if matchstr(getline(5), '\S') > ''
          execute 'normal Yp'
        endif
        execute 'startinsert'
      endfunction

      " position cursor for email reply or new message, see .sup/config.yaml and bin/dcompose
      autocmd Filetype mail setlocal spell wrap enc=utf-8 formatoptions=tqwan1 textwidth=72 syntax=mail
      autocmd Filetype mail call ComposeMail()

" files.vim
