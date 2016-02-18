" sdothum - 2016 (c) wtfpl

" Functions
" ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

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

      " sort compare by numeric (not string) value
      function! s:CompareNumber(i1, i2)
        return a:i1 - a:i2
      endfunction

      function! SortNumbers(nums)
        return sort(a:nums, '<SID>CompareNumber')
      endfunction


  " Buffers ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ...................................................... Filetype attributes

      " [regex name, filetype, modifiable, wordcount] rule tuple
      " modifiable (0) nomodifiable (1) modifiable
      " wordcount (0) no word count (1) statusline with wordcount
      " note "/name" to represent "^name"
      let s:nametypes=[
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
      let s:contenttypes=[
        \  ['binary', 'binary', 0, 1]
        \, ['no read permission', 'binary', 0, 1]
        \, ['text', 'text', 1, 0]
        \]

      " set buffer attributes
      function! CheckFiletype()
        let b:prose=0
        " by name structure
        for [name, filetype, modifiable, wordcount] in s:nametypes
          " known filetypes can be processed by filename e.g. "mkd" readme files
          if expand('%') =~ name
            " assign unknown filetype
            let &filetype=(&filetype == '' ? filetype : &filetype)
            let &modifiable=modifiable
            let b:prose=wordcount
            break
          endif
        endfor
        " by file content if not autodetected
        if &filetype == ''
          for [content, filetype, modifiable, readonly] in s:contenttypes
            if system('file -i ' . expand('%') . '|cut -d: -f2') =~ content
              let &filetype=filetype
              let &modifiable=modifiable
              let &readonly=readonly
            endif
          endfor
        endif
        " make nomodifiable persistent, see toggle below
        let b:modifiable=&modifiable
      endfunction

      " toggle modifiable attribute
      nmap <silent><F1>   :let &modifiable=(b:modifiable == 1 ? (&modifiable == 0 ? 1 : 0) : b:modifiable)<CR>
      nmap <silent><C-F1> :let &modifiable=(&modifiable == 0 ? 1 : 0)<CR>

      " check filetype on open
      autocmd BufNewFile,BufRead * call CheckFiletype()
      autocmd BufWinEnter        *.txt,*.txt.gz if &filetype == 'help' | set nomodifiable | let b:prose=1 | endif

    " .................................................................... Wikis

      function! Wiki(index)
        " open vimwiki index file
        execute 'normal ' . a:index . ',ww'
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
        call HiLite()
        redir END
      endfunction

      " open vimwiki ()
      autocmd VimEnter * if argv (0) == 'thedarnedestthing' | bdelete | call OpenWikis() | endif
      autocmd VimEnter * if &filetype == 'vimwiki' | call ToggleGoyo(0) | endif

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
      autocmd VimEnter * if &filetype == 'mail' | call ToggleGoyo(0) | endif

  " UI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Solarized

      " further distraction free mode settings
      " let s:dfm_fg='#586e75'                " solarized foreground
      " let s:dfm_fg_dark='#93a1a1'           " solarized light foreground
      let s:dfm_fg='#333333'                " dark foreground
      let s:dfm_fg_dark='#cccccc'           " light foreground
      let s:dfm_bg='#fdf6e3'                " solarized light (paper) background
      let s:dfm_bg_dark='#002b36'           " solarized dark background
      let s:dfm_cursor='#15ABDD'            " ia writer blue cursor
      let s:dfm_cursor_dark='#DA4716'       " reddish cursor
      let s:dfm_unfocused='#93A1A1'         " light grey surrounding text content
      let s:dfm_unfocused_dark='#576565'    " dark grey surrounding text content
      let s:dfm_fg_line='#cccccc'           " light grey line numbers
      let s:dfm_fg_line_dark='#444444'      " dark grey line numbers
      let s:dfm_bg_line='#eee8d5'           " solarized light cursorline
      let s:dfm_bg_line_dark='#073642'      " solarized dark cursorline

      " match marks margin and whitespace colours to background
      function! LiteBackground()
        if &background == 'light'
          execute 'highlight ShowMarksHLl    guibg=' . s:dfm_bg
          execute 'highlight SignColumn      guibg=' . s:dfm_bg
          execute 'highlight ExtraWhitespace guibg=' . s:dfm_cursor_dark . ' guifg=' . s:dfm_fg_dark
          execute 'highlight InsertCursor    guibg=' . s:dfm_cursor      . ' guifg=' . s:dfm_fg
          execute 'highlight VisualCursor    guibg=' . s:dfm_cursor_dark . ' guifg=' . s:dfm_fg
          execute 'highlight ReplaceCursor   guibg=' . s:dfm_cursor_dark . ' guifg=' . s:dfm_fg
          execute 'highlight CommandCursor   guibg=' . s:dfm_cursor_dark . ' guifg=' . s:dfm_fg
        else
          execute 'highlight ShowMarksHLl    guibg=' . s:dfm_bg_dark
          execute 'highlight SignColumn      guibg=' . s:dfm_bg_dark
          execute 'highlight ExtraWhitespace guibg=' . s:dfm_cursor      . ' guifg=' . s:dfm_fg
          execute 'highlight InsertCursor    guibg=' . s:dfm_cursor_dark . ' guifg=' . s:dfm_fg
          execute 'highlight VisualCursor    guibg=' . s:dfm_cursor      . ' guifg=' . s:dfm_fg
          execute 'highlight ReplaceCursor   guibg=' . s:dfm_cursor      . ' guifg=' . s:dfm_fg
          execute 'highlight CommandCursor   guibg=' . s:dfm_cursor      . ' guifg=' . s:dfm_fg
        end
      endfunction

    " ............................................................ LiteDFM views

      " source code style
      function! CodeView()
        execute 'LiteDFM'
        set showmode
        set laststatus=2                    " turn on statusline
        call LiteBackground()
        if &background == 'light'
          execute 'highlight LineNr guifg=' . s:dfm_fg_line
          execute 'highlight CursorLineNr guibg=' . s:dfm_bg_line
        else
          execute 'highlight LineNr guifg=' . s:dfm_fg_line_dark
          execute 'highlight CursorLineNr guibg=' . s:dfm_bg_line_dark
        end
      endfunction

      " vimwiki prose style
      function! ProseView()
        if exists('#goyo')
          call s:GoyoEnter()
        else
          call s:GoyoLeave()
          execute 'LiteDFM'
        endif
        set colorcolumn=0
        set noshowmode
        call LiteBackground()
        call HiLite()
        " hide line numbers
        if &background == 'light'
          execute 'highlight Normal guifg=' . s:dfm_unfocused
          execute 'highlight CursorLineNr guifg=' . s:dfm_bg . ' guibg=' . s:dfm_bg
        else
          execute 'highlight Normal guifg=' . s:dfm_unfocused_dark
          execute 'highlight CursorLineNr guifg=' . s:dfm_bg_dark . ' guibg=' . s:dfm_bg_dark
        end
        " persistent word count display, see ToggleStatus
        let &laststatus=g:wikistatus
      endfunction

      function! LiteType()
        if &filetype =~ 'vimwiki\|mail'
          call ProseView()
        else
          call CodeView()
        endif
      endfunction

      " intial view mode: source code or prose
      autocmd BufEnter * call LiteType()

    " ................................................................ Goyo view

      " goyo initialization hooks
      " undo lite-dfm settings
      function! s:GoyoEnter()
        " silent !tmux set status off
        set scrolloff=999
        set numberwidth=1
        set foldcolumn=0
        set nonumber
      endfunction

      " reset vimwiki link color
      function! s:GoyoLeave()
        " silent !tmux set status on
        set scrolloff=8
        set number
        " restore vimwiki link
        call VimWikiLink()
      endfunction

      autocmd User GoyoEnter nested call <SID>GoyoEnter()
      autocmd User GoyoLeave nested call <SID>GoyoLeave()

      " toggle goyo / litedfm
      " offset (0) new or resized window (1) current window
      function! ToggleGoyo(offset)
        if &filetype =~ 'vimwiki\|mail'
          if !exists('#goyo')
            execute 'LiteDFMClose'
            " width must be greater than textwidth
            if exists('s:goyo') && a:offset > 0
              execute 'Goyo ' . (&textwidth + 1) . '+6'
            else
              execute 'Goyo ' . (&textwidth + 1) . '+1'
            endif
            " subsequent goyo toggling alters left margin position
            let s:goyo=1
          else
            execute 'Goyo!'
            call ProseView()
          endif
          " force spellcheck as autocmd sequences don't seem to set this consistently
          set spell
        endif
      endfunction

      imap <S-F7> <C-o>:call ToggleGoyo(1)<CR>
      nmap <S-F7> :call ToggleGoyo(1)<CR>

      " reset window margins by toggling goyo on and off (<C-w>= leaves number artifacts)
      function! ResetGoyo(offset)
        if exists('#goyo')
          " goyo! always returns to first buffer, so remember last
          let l:buffer=bufnr('%')
          call ToggleGoyo(a:offset)
          execute 'buffer ' . l:buffer
          call ToggleGoyo(a:offset)
        endif
      endfunction

      " reset margins in current window
      imap <F7> <C-o>:call ResetGoyo(1)<CR>
      nmap <F7> :call ResetGoyo(1)<CR>

      " with window resizing, goyo margins are newly calculated
      autocmd VimResized * if &filetype =~ 'vimwiki\|mail' | call ResetGoyo(0) | endif

    " ............................................................. Colour theme

      " restore vimwiki link
      function! VimWikiLink()
        highlight VimwikiLink guifg=#268bd2 gui=bold
      endfunction

      " toggle colour scheme
      function! LiteSwitch()
        " 1st usage causes one time initialization error, trap error instead
        call Quietly('LiteDFMClose')
        let &background=(&background == 'dark' ? 'light' : 'dark')
        call LiteType()
        if exists('#goyo')
          call ToggleHiLite()
        else
          " match lightline to current colorscheme, see https://github.com/itchyny/lightline.vim/issues/104
          if &background == 'light'
            let g:lightline.colorscheme = 'solarized_light'
          else
            let g:lightline.colorscheme = 'solarized_dark'
          end
          call lightline#init()
          call lightline#colorscheme()
          call lightline#update()
        endif
        " restore vimwiki link
        call VimWikiLink()
      endfunction

      imap <silent><F9> <C-o>:call LiteSwitch()<CR>
      nmap <silent><F9> :call LiteSwitch()<CR>

      " set prose cursorline theme
      function! HiLite()
        if !exists('s:cursorline')
          call ToggleHiLite()
        endif
        execute 'highlight CursorLine gui=bold guibg='   . s:cursorline       . ' guifg=' . s:foreground
        if &background == 'light'
          execute 'highlight CursorLineNr guibg='        . s:dfm_bg
          execute 'highlight Cursor guibg='              . s:dfm_cursor       . ' guifg=' . s:dfm_bg
        else
          execute 'highlight CursorLineNr guibg='        . s:dfm_bg_dark
          execute 'highlight Cursor guibg='              . s:dfm_cursor_dark  . ' guifg=' . s:dfm_bg_dark
        end
      endfunction

      " cursorline contrast (0) low (1) high
      let s:contrast=1

      function! CursorLine(fg, bg, BG)
        let s:foreground=a:fg
        if s:cursorline != a:bg
          let s:cursorline=a:bg
        else
          let s:cursorline=a:BG
        endif
      endfunction

      function! ToggleHiLite()
        if &filetype =~ 'vimwiki\|mail'
          if !exists('s:cursorline')
            let s:cursorline='#000000'
          endif
          if s:contrast == 0
            " low contrast cursorline
            if &background == 'light'
              call CursorLine(s:dfm_fg, s:dfm_bg, s:dfm_bg_line)
            else
              call CursorLine(s:dfm_fg_dark, s:dfm_bg_dark, s:dfm_bg_line_dark)
            end
          else
            " high contrast cursorline
            if &background == 'light'
              call CursorLine(s:dfm_fg, s:dfm_bg_line, s:dfm_bg)
            else
              call CursorLine(s:dfm_fg_dark, s:dfm_bg_line_dark, s:dfm_bg_dark)
            end
          endif
          call HiLite()
        endif
      endfunction

      imap <silent><S-F9> <C-o>:call ToggleHiLite()<CR>
      nmap <silent><S-F9> :call ToggleHiLite()<CR>

    " ..................................................................... Font

      function! FontSwitch()
        " adjust font displays for various gpu's
        if system("lspci") =~ 'VGA .* NVIDIA'
          " for macbook nvidia gpu
          if &guifont == 'PragmataPro 11'
            set guifont=Fantasque\ Sans\ Mono\ 11
            execute 'set linespace=' . (argv(0) == 'thedarnedestthing' || expand('%:t') =~ '\(wiki\|eml\)$' ? 18 : 7)
          else
            set guifont=PragmataPro\ 11
            execute 'set linespace=' . (argv(0) == 'thedarnedestthing' || expand('%:t') =~ '\(wiki\|eml\)$' ? 16 : 5)
          endif
        else
          " for ati/intel gpu's
          if &guifont == 'PragmataPro 12'
            set guifont=Fantasque\ Sans\ Mono\ 12
            execute 'set linespace=' . (argv(0) == 'thedarnedestthing' || expand('%:t') =~ '\(wiki\|eml\)$' ? 18 : 7)
          else
            set guifont=PragmataPro\ 12
            execute 'set linespace=' . (argv(0) == 'thedarnedestthing' || expand('%:t') =~ '\(wiki\|eml\)$' ? 16 : 5)
          endif
        endif
      endfunction

      imap <silent><C-F9> <C-o>:call FontSwitch()<CR>
      nmap <silent><C-F9> :call FontSwitch()<CR>

    " ....................................................... Trailing highlight

      " toggle trailing whitespace highlight
      function! ToggleSpaces()
        set list!
        if &list == 0
          match ExtraWhitespace /\%x00$/    " nolist by failing match with null character :-)
          echo ''
        else
          match ExtraWhitespace /\s\+$/
          echo 'List invisibles ON'
        end
      endfunction

      imap <silent><F8> <C-o>:call ToggleSpaces()<CR>
      nmap <silent><F8> :call ToggleSpaces()<CR>

    " ................................................. Colorcolumns / line wrap

      let s:linewidth=&textwidth            " default to &textwidth, see Ruler
      " column list must end in [0]
      let s:margins=[45, 72, &textwidth, s:linewidth, 0]

      " toggle colorcolumns
      function! ToggleColumn()
        " add 1st non-blank character in current line to margins list :-)
        if getline(line('.')) != ''
          if index(s:margins, col('.')) == -1
            let s:margins=uniq(SortNumbers([col('.')]+s:margins[:-2])) + [0]
            let &colorcolumn=col('.')
            return
          endif
        endif
        execute 'let l:index=index(s:margins,' . &colorcolumn . ') + 1'
        " weird problem with if test so simply loop list! :-o
        " if l:index > len(s:margins)
        "   let l:index=0
        " endif
        " let &colorcolumn=s:margins[l:index]
        let &colorcolumn=(s:margins+s:margins)[l:index]
      endfunction

      imap <S-F11> <C-o>:call ToggleColumn()<CR>
      nmap <S-F11> :call ToggleColumn()<CR>

      " toggle line wrap
      function! ToggleWrap()
        if &formatoptions == 'tqwan1'
          " NoPencil
          set formatoptions=qwn1
          " echo PencilMode() . ' - Automatic line wrap OFF'
          echo 'Automatic line wrap OFF'
        elseif &formatoptions == 'qwn1'
          " Pencil
          set formatoptions=tqwan1
          " echo PencilMode() . ' - Automatic line wrap ON'
          echo 'Automatic line wrap ON'
        endif
      endfunction

      imap <silent><F4> <C-o>:call ToggleWrap()<CR>
      nmap <silent><F4> :call ToggleWrap()<CR>

      " " toggle pencil
      " function! TogglePencil()
      "   TogglePencil
      "   echo PencilMode()
      " endfunction
      "
      " imap <C-F4> <C-o>:call TogglePencil()<CR>
      " nmap <C-F4> :call TogglePencil()<CR>

  " Editing ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Select text

      function! ParagraphAbove()
        if matchstr(getline(line('.')), '\S') == ''
          normal {
          if matchstr(getline(line('.')), '\S') == ''
            normal j
          endif
        endif
        normal }kV{
      endfunction

      function! ParagraphBelow()
        if matchstr(getline(line('.')), '\S') == ''
          normal }
          if matchstr(getline(line('.')), '\S') == ''
            normal k
          endif
        endif
        normal {jV}
      endfunction

      " select paragragh
      nmap <A-PageUp>   :call ParagraphAbove()<CR>
      nmap <A-PageDown> :call ParagraphBelow()<CR>
      " extend paragraph selection
      vmap <A-PageUp>   {
      vmap <A-PageDown> }

    " ............................................................. Comment text

      " toggle comment, see T-comment plugins.vim
      function! ToggleComment()
        let l:col=virtcol('.')
        " suffix empty line from successive ToggleComment's
        " (for cycles: empty commented -> uncommented -> empty commented..)
        if matchstr(getline(line('.')), '\s') > ''
          let l:mark=l:col
          normal aMark
        else
          let l:mark=0
        endif
        " comment line
        normal ,c
        " reposition cursor when uncommenting autocomment line (creates "" line)
        if matchstr(getline(line('.')), '\S') == ''
          execute 'normal ' . l:col . 'a '
          execute "normal a\<BS>"
        else
          normal $
          " remove empty comment suffix
          if l:mark > 0
            normal xxxx
          endif
        endif
      endfunction

      imap <leader>c <C-o>:call ToggleComment()<CR>

    " .......................................................... Code block text

      " convert wiki text lines into code block lines
      function! CodeBlock()
        execute "silent! normal :s/\\(.*\\)/`\\1`/\<CR>"
        " preserve leading spaces with wiki markdown
        execute "silent! normal gv:s/^` /`^ /\<CR>"
        execute "silent! normal gv:s/^``/`^ `/e\<CR>"
        " convert [[ test ]], see thedarnedestthing markdown
        execute "silent! normal gv:s/ \\[\\[ / [[] /e\<CR>"
        execute "silent! normal gv:s/ \\]\\] / []] /e\<CR>"
      endfunction

    " ...................................................... Vertical text shift

      " see editing.vim for left/right key mappings
      " see https://github.com/gorkunov/vimconfig.git
      function! s:MoveLineUp()
        call s:MoveLineOrVisualUp('.', '')
      endfunction

      function! s:MoveLineDown()
        call s:MoveLineOrVisualDown('.', '')
      endfunction

      function! s:MoveVisualUp()
        call s:MoveLineOrVisualUp("'<", "'<,'>")
        normal gv
      endfunction

      function! s:MoveVisualDown()
        call s:MoveLineOrVisualDown("'>", "'<,'>")
        normal gv
      endfunction

      function! s:MoveLineOrVisualUp(line_getter, range)
        let l_num=line(a:line_getter)
        if l_num - v:count1 - 1 < 0
          let move_arg='0'
        else
          let move_arg=a:line_getter . ' -' . (v:count1 + 1)
        endif
        call s:MoveLineOrVisualUpOrDown(a:range . 'move ' . move_arg)
      endfunction

      function! s:MoveLineOrVisualDown(line_getter, range)
        let l_num=line(a:line_getter)
        if l_num + v:count1 > line('$')
          let move_arg='$'
        else
          let move_arg=a:line_getter . ' +' . v:count1
        endif
        call s:MoveLineOrVisualUpOrDown(a:range . 'move ' . move_arg)
      endfunction

      function! s:MoveLineOrVisualUpOrDown(move_arg)
        let col_num=virtcol('.')
        execute 'silent! ' . a:move_arg
        execute 'normal! ' . col_num . '|'
      endfunction

      " shift text up / down
      imap <silent><S-Up>   <ESC>:call <SID>MoveLineUp()<CR>a
      imap <silent><S-Down> <ESC>:call <SID>MoveLineDown()<CR>a
      nmap <silent><S-Up>   <ESC>:call <SID>MoveLineUp()<CR>
      nmap <silent><S-Down> <ESC>:call <SID>MoveLineDown()<CR>
      vmap <silent><S-Up>   <ESC>:call <SID>MoveVisualUp()<CR>
      vmap <silent><S-Down> <ESC>:call <SID>MoveVisualDown()<CR>

  " Line styles ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Underline

      " regex list of multibyte characters used for line drawing, see LineInfo
      let s:multibytes='[─═▬]'

      " insert underline
      function! Underline(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          normal ^
          let l:col=virtcol('.')
          execute 'normal yypVr' . a:delimiter
          " blank to margin
          if l:col > 1
            execute 'normal ' . l:col . 'r '
          endif
          " prefix lua/haskell comment to avoid potential delimiter conflict
          if &filetype == 'lua' || &filetype == 'haskell'
            normal ^iMark
          endif
          " comment line
          normal ,c
          if &filetype == 'lua' || &filetype == 'haskell'
            normal /Mark
            " remove prefix+1 to treat as common 2 byte comment leader
            normal 4x$x
          endif
          " place delimiter immediately after comment header (no space)
          " if &filetype != 'lua' && &filetype != 'haskell' && a:delimiter != '='
          "   execute 'normal ^f r' . a:delimiter
          " endif
          " adjust delimiter length by comment leader
          if l:col == 1
            normal $xx
          else
            normal 0x$x
          endif
          normal ^
        endif
      endfunction

      imap <leader>- <C-o>:call Underline('▔')<CR><C-Return>
      nmap <leader>- :call Underline('▔')<CR><Down>
      imap <leader>= <C-o>:call Underline('▬')<CR><C-Return>
      nmap <leader>= :call Underline('▬')<CR><Down>
      " imap <leader><leader>+ <C-o>:call Underline('▬')<CR><C-Return>
      " nmap <leader><leader>+ :call Underline('▬')<CR><Down>

    " .................................................................... Ruler

      " insert ruler
      " note: to insert a ruler on an empty line (virtual column position), add a <space> character,
      " else in insert mode, the ruler will positioned using column 1
      function! Drawline(delimiter)
        " insert dummy mark line if on blank line
        if matchstr(getline(line('.')), '\S') == ''
          let l:mark=1
          normal $RMark
        else
          let l:mark=0
        endif
        call Underline(a:delimiter)
        " remove temporary mark
        if l:mark > 0
          normal kdd
        endif
        normal $
        if virtcol('.') < s:linewidth
          " for mirrored left/right margin spacing
          " let l:col=s:linewidth - virtcol('.') - l:col + 1
          let l:col=s:linewidth - virtcol('.')
          execute 'normal ' . l:col . 'a' . a:delimiter
        endif
        normal ^
      endfunction

      " imap <leader><leader>- <C-o>:call Drawline('─')<CR>
      " nmap <leader><leader>- :call Drawline('─')<CR>
      imap <leader><leader>- <C-o>:call Drawline('▔')<CR>
      nmap <leader><leader>- :call Drawline('▔')<CR>
      " imap <leader><leader>= <C-o>:call Drawline('═')<CR>
      " nmap <leader><leader>= :call Drawline('═')<CR>
      imap <leader><leader>= <C-o>:call Drawline('▬')<CR>
      nmap <leader><leader>= :call Drawline('▬')<CR>
      imap <leader><leader>: <C-o>:call Drawline(':')<CR>
      nmap <leader><leader>: :call Drawline(':')<CR>

    " .................................................................. Trailer

      " append trailer
      function! AppendTrailer(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing trailer
          if matchstr(getline(line('.')), '\s[' . a:delimiter . ']\+$') > ''
            " normal $bhD
            normal $bmD
          endif
          normal $
          let l:col=s:linewidth - virtcol('.') - 1
          if l:col > 0
            " suppress potential comment line wrapping
            set formatoptions-=c
            execute 'normal a '
            execute 'normal ' . l:col . 'a' . a:delimiter
            set formatoptions+=c
          endif
          normal ^
        endif
      endfunction

      imap <leader><leader>_ <C-o>:call AppendTrailer('▁')<CR>
      nmap <leader><leader>_ :call AppendTrailer('▁')<CR>
      imap <leader><leader>. <C-o>:call AppendTrailer('.')<CR>
      nmap <leader><leader>. :call AppendTrailer('.')<CR>

      " prompted trailer
      function! InputTrailer()
        let l:delimiter=input('Line character: ')
        if l:delimiter > ''
          call AppendTrailer(l:delimiter[0])
        endif
      endfunction

      imap <leader><leader>? <C-o>:call InputTrailer()<CR>
      nmap <leader><leader>? :call InputTrailer()<CR>

    " ................................................................... Leader

      " insert leader
      function! InsertLeader(delimiter)
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing leader
          if matchstr(getline(line('.')), '\S\s\+[' . a:delimiter . ']\+\s') > ''
            " execute 'normal ^wdf '
            execute 'normal ^wdt x'
          endif
          call AppendTrailer(a:delimiter)
          " cut trailer and insert as leader!
          " normal $bhD^whP
          normal $bmD^wmP
          normal ^
        endif
      endfunction

      imap <leader>_ <C-o>:call InsertLeader('▁')<CR>
      nmap <leader>_ :call InsertLeader('▁')<CR>
      imap <leader>. <C-o>:call InsertLeader('.')<CR>
      nmap <leader>. :call InsertLeader('.')<CR>

      " prompted leader
      function! InputLeader()
        let l:delimiter=input('Line character: ')
        if l:delimiter > ''
          if l:delimiter == ' '
            call RightJustify()
          else
            call InsertLeader(l:delimiter[0])
          endif
        endif
      endfunction

      imap <leader>? <C-o>:call InputLeader()<CR>
      nmap <leader>? :call InputLeader()<CR>

      function! RightJustify()
        if matchstr(getline(line('.')), '\S') > ''
          " remove existing leader
          if matchstr(getline(line('.')), '^ *') > ''
            " execute 'normal 0vwhd'
            execute 'normal 0vwmd'
          endif
          normal $
          let l:col=s:linewidth - virtcol('.') - 1
          if l:col > 0
            " suppress potential comment line wrapping
            set formatoptions-=c
            normal ^
            execute 'normal ' . l:col . 'i '
            execute 'normal a '
            set formatoptions+=c
          endif
        endif
      endfunction

      imap <leader><leader><Space> <C-o>:call RightJustify()<CR>
      nmap <leader><leader><Space> :call RightJustify()<CR>

  " Statusline ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ................................................................ Line info

      " see https://github.com/scrooloose/vimfiles/blob/master/vimrc#L78
      " return a warning for long lines > s:linewidth
      function! s:LineInfo()
        " return 'x +/y z' if long lines are found, where
        "   x is the number of long lines
        "   y is the median length of the long lines
        "   z is the length of the longest line
        if !exists('b:statusline_long_line_warning')
          let b:statusline_long_line_warning=''
          if &modifiable
            let long_line_lens=s:LongLines()
            if len(long_line_lens) > 0
              let b:statusline_long_line_warning=
                \           len(long_line_lens) . ':'
                \ . ' +/' . s:Median(long_line_lens)
                \ . ' '   . max(long_line_lens)
            else
              let b:statusline_long_line_warning=''
            endif
          endif
        endif
        return b:statusline_long_line_warning
      endfunction

      " return a list containing the lengths of the long lines in this buffer
      function! s:LongLines()
        let l:spaces=repeat(' ', &tabstop)
        " let l:line_lens=map(getline(1,'$'), 'len(substitute(v:val, "\\t", l:spaces, "g"))')
        " trap multibyte line drawing characters used by "ruler" and "underline"
        " let l:line_lens=map(getline(1,'$'), 'v:val =~ s:multibytes
        "   \ ? len(substitute(substitute(v:val, s:multibytes, " ", "g"), "\\t", l:spaces, "g"))
        "   \ : len(substitute(v:val, "\\t", l:spaces, "g"))')
        let l:line_lens=map(getline(1,'$'), 'len(substitute(v:val =~ s:multibytes ? substitute(v:val, s:multibytes, " ", "g") : v:val, "\\t", l:spaces, "g"))')
        return filter(l:line_lens, 'v:val > s:linewidth')
      endfunction

      " find the median of the given array of numbers
      function! s:Median(nums)
        " original code incorrectly sorted by text
        let l:nums=SortNumbers(a:nums)
        " echo l:nums
        let l:size=len(l:nums)
        if l:size % 2 == 1
          let l:middle=(l:size-1)/2
          return l:nums[l:middle]
        else
          return (l:nums[l:size/2] + l:nums[(l:size/2)-1]) / 2
        endif
      endfunction

      function! LineInfo()
        " plugin command windows bypass autocmds
        if exists('b:code')
          if b:code == 1
            return s:LineInfo()
          endif
        endif
        return ''
      endfunction

      " line statistics off by default per buffer
      autocmd bufread                 * let b:code=0
      " recalculate the long line warning when idle and after saving
      autocmd cursorhold,bufwritepost * unlet! b:statusline_long_line_warning

    " ............................................................... Word count

      " persistent vimwiki wordcount statusline
      let g:wikistatus=0                    " default vimwiki statusline off

      " see http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
      " null return suppresses wordcount for non-prose or empty new buffer
      function! s:WordCount()
        let b:wordcount=''
        let l:statusmsg=v:statusmsg
        " g<C-g> prevents (cursor from) appending to EOL in vim 7.4
        let l:position=getpos('.')
        execute "silent normal g\<C-g>"
        if v:statusmsg != '--No lines in buffer--'
          let b:wordcount=str2nr(split(v:statusmsg)[11])
        endif
        let v:statusmsg=l:statusmsg
        " go back (to EOL if need be)
        call setpos('.', l:position)
        return b:wordcount
      endfunction

      function! WordCount()
        " plugin command windows bypass autocmds
        if exists('b:prose')
          " ignore source code word counts
          if b:prose == 1
            return s:WordCount()
          endif
        endif
        return ''
      endfunction

      " toggle word count manually
      imap <silent><A-F10> <C-o>:let b:prose=(b:prose == 0 ? 1 : 0)<CR>
      nmap <silent><A-F10> :let b:prose=(b:prose == 0 ? 1 : 0)<CR>

    " ........................................................ Special Character

      function! SpecialChar()
        " getline() test fails on switch into insert mode
        if mode() == 'n'
          " ignore newline (is NUL)
          if getline(line('.')) != ''
            let l:char=getline('.')[col('.')-1]
            " not interested in ascii keyboard characters
            if l:char !~ '\(\d\|\a\|\s\|[`~!@#$%^&*()_\-+={}\[\]\\|;:\",\.<>/?]\)' && l:char != "'"
              let l:statusmsg=v:statusmsg
              normal ga
              " show hex value :-)
              let l:hex='\x' . matchstr(split(v:statusmsg)[3], '[^,]*')
              let v:statusmsg=l:statusmsg
              " clear ga information!
              echo ''
              return l:hex
            endif
          endif
        endif
        return ''
      endfunction

    " ................................................................. Warnings

      " see https://github.com/scrooloose/vimfiles/blob/master/vimrc#L78
      " inconsistent tab warning
      function! Indent()
        " return '&expandtab' if &expandtab is set wrong
        " return '^ indent' if spaces and tabs are used to indent
        if !exists('b:statusline_tab_warning')
          let b:statusline_tab_warning=''
          if &modifiable
            let l:tabs=search('^\t', 'nw') != 0
            "find spaces that arent used as alignment in the first indent column
            let l:spaces=search('^ \{' . &tabstop . ',}[^\t]', 'nw') != 0
            if l:tabs && l:spaces
              let b:statusline_tab_warning='^ indent'
            elseif (l:spaces && !&expandtab) || (l:tabs && &expandtab)
              let b:statusline_tab_warning='&expandtab'
            endif
          endif
        endif
        return b:statusline_tab_warning
      endfunction

      " trailing spaces warning
      function! Spaces()
        " return 'spaces $' if trailing spaces/tabs are present
        if !exists('b:statusline_pad_warning')
          let b:statusline_pad_warning=''
          if &modifiable
            if exists('b:prose')
              if b:prose == 0
                if search('[ \t]\+$', 'nw') != 0
                  let b:statusline_pad_warning='spaces $'
                endif
              endif
            endif
          endif
        endif
        return b:statusline_pad_warning
      endfunction

      " recalculate the tab warning flag when idle and after writing
      autocmd cursorhold,bufwritepost * unlet! b:statusline_tab_warning
      " recalculate the trailing whitespace warning when idle, and after saving
      autocmd cursorhold,bufwritepost * unlet! b:statusline_pad_warning

    " ......................................................... Statusbar format

      " toggle vimwiki word count in statusline (0) current buffer (1) all buffers
      " or
      " toggle coding line statistics
      function! ToggleStatus(persistence)
        " show/hide word count info
        if &filetype =~ 'vimwiki\|mail'
          if a:persistence == 0
            let &laststatus=(&laststatus == 0 ? 2 : 0)
          else
            let g:wikistatus=(g:wikistatus == 0 ? 2 : 0)
            let &laststatus=g:wikistatus
          endif
          " turn off persistence whenever statusline turned off :-)
          if &laststatus == 0
            let g:wikistatus=0
          endif
        " show/hide line statistics
        else
          " toggle line info statistics where word counts are inapplicable
          if b:prose == 0
            let b:code=(b:code == 0 ? 1 : 0)
          endif
        endif
        " clear show message
        echo ''
      endfunction

      imap <F10>   <C-o>:call ToggleStatus(0)<CR>
      nmap <F10>   :call ToggleStatus(0)<CR>
      imap <S-F10> <C-o>:call ToggleStatus(1)<CR>
      nmap <S-F10> :call ToggleStatus(1)<CR>

" functions.vim
