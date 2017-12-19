" sdothum - 2016 (c) wtfpl

" Core
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " Primitive ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .................................................................... Setup

      if exists("g:loaded_core")
        finish
      endif
      let g:loaded_core = 1
      let s:save_cpo = &cpo
      set cpo&vim

      let g:matchspace = ''                 " see ToggleSpaces() gui.vim
      let g:ruler      = 0                  " colorcolumn mode

      augroup core
        autocmd!
      augroup END

    " ....................................................... Error message trap

      " ignore 1st time error messages from plugins (uninitialized s:variables)
      function! Quietly(command)
        try
          execute a:command
        catch /.*/
          " discard messages, do nothing
        endtry
      endfunction

    " ............................................................. Numeric sort

      function! SortNumbers(nums)
        return sort(a:nums, 'core#CompareNumber')
      endfunction

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................ Open terminal

      " open shell session in buffer directory
      nmap <silent><leader>te :silent call system('term "vimterm" STACK')<CR>

    " ............................................................... Print file

      nmap <silent><leader>ha :silent call core#Hardcopy()<CR>:echo 'Printing..'<CR>

  " Keyboard layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Colemak-shift-dh

      " environment variable
      if $COLEMAK == 'true'
        call core#Colemak()
      endif

  " GUI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................... Gvim Options (make it look like terminal!)

      " toggle gui menu
      function! ToggleGui()
        if &guioptions =~# 'T'
          set guioptions-=T
          set guioptions-=m
        else
          set guioptions+=T
          set guioptions+=m
        endif
      endfunction

      function! RefreshGui()
        call ToggleGui()
        call ToggleGui()
      endfunction

      " Toggle Menu and Toolbar
      nnoremap <silent><F12>       :call ToggleGui()<CR>
      inoremap <silent><F12>       <C-o>:call ToggleGui()<CR>

    " ........................................................... Column margins

      nmap <silent><Bar>           :call core#ToggleColumn()<CR>
      nmap <silent><leader><Bar>   :IndentGuidesToggle<CR>:call IndentTheme()<CR>

    " ............................................................. Line numbers

      nmap <silent>#               :call core#ToggleNumber()<CR>

    " ...................................................... White space markers

      function! MatchSpace()
        return g:matchspace
      endfunction

      nmap <silent><leader><Space> :call core#ToggleSpaces()<CR>

  " Buffer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................. Buffer count

      function! BufCount()
        return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
      endfunction

    " ................................................................ Line wrap

      nmap <silent><leader><CR> :call core#ToggleWrap()<CR>

    " ......................................................... Strip whitespace

      " see https://dougblack.io/words/a-good-vimrc.html
      " strips trailing whitespace from all lines
      function! StripTrailingWhitespaces()
        if &modifiable == 1 && ! Markdown()
          " save last search & cursor position
          let l:_s = @/
          let l:l  = line(".")
          let l:c  = col(".")
          %s/\s\+$//e
          let @/ = l:_s
          call cursor(l:l, l:c)
        endif
      endfunction

  " Text ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Select text

      " select paragragh
      nmap <silent><A-PageUp>     :call core#ParagraphAbove()<CR>
      nmap <silent><A-PageDown>   :call core#ParagraphBelow()<CR>

    " .......................................................... Shift up / down

      " shift text up / down
      imap <silent><S-Up>         <ESC>:call core#MoveLineUp()<CR>a
      imap <silent><S-Down>       <ESC>:call core#MoveLineDown()<CR>a
      nmap <silent><S-Up>         :call core#MoveLineUp()<CR>
      nmap <silent><S-Down>       :call core#MoveLineDown()<CR>
      vmap <silent><S-Up>         <ESC>:call core#MoveVisualUp()<CR>
      vmap <silent><S-Down>       <ESC>:call core#MoveVisualDown()<CR>

    " ......................................................... Insert line wrap

      inoremap <silent><C-Return> <C-o>:call core#InsertWrap()<CR>

    " .......................................................... Code block text

      " markup wiki code blocks
      nnoremap <silent><leader>`  V:call core#CodeBlock()<CR>
      vmap     <silent><leader>`  :call core#CodeBlock()<CR>

  " Filetype ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Prose filestypes

      " distraction free filetyes
      function! Prose()
        return &filetype =~ 'vimwiki\|wiki\|mail\|markdown\|draft'
      endfunction

      function! Markdown()
        return &filetype =~ 'vimwiki\|wiki\|markdown'
      endfunction

    " ............................................................... Modifiable

      " check filetype on open
      autocmd core BufNewFile,BufRead * call core#CheckFiletype()

    " ................................................................... E-mail

      " position cursor for email reply or new message, see .sup/config.yaml and bin/dcompose
      autocmd core Filetype mail call core#ComposeMail()

    " ......................................................... Vimwiki markdown

      " reformat vimwiki markdown table
      nmap <silent><leader><leader>v :silent call core#ReformatVimwikiTable()<CR>

      let &cpo = s:save_cpo
      unlet s:save_cpo

" core.vim
