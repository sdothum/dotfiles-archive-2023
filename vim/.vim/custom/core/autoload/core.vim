" sdothum - 2016 (c) wtfpl

" Primitive
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................................................... Redraw gui

      let s:delay = '100m'                  " redraw delay, see ui#FontSize()

      " toggle in/out to fill window
      function! core#RedrawGui()
        execute 'sleep ' . s:delay
        call core#ToggleGui()
        call core#ToggleGui()
      endfunction

    " .............................................................. Reload vim

      function! core#Vimrc()
        " after editing vim configs
        execute 'wall'
        autocmd!
        source $MYVIMRC
        call ui#Retheme()
      endfunction

    " ............................................................. Numeric sort

      " sort compare by numeric (not string) value
      function! core#CompareNumber(i1, i2)
        return a:i1 - a:i2
      endfunction

      function! core#SortNumbers(nums)
        return sort(a:nums, 'core#CompareNumber')
      endfunction

    " ............................................................... Print file

      " latex printing
      function! core#Hardcopy()
        if core#Markdown()
          execute '!hardcopy wiki \"' . expand('%:t') . '\"'
        elseif expand('%:p') =~ 'Patricia'
          execute '!hardcopy wps' expand('%:t')
        else
          execute '!hardcopy code' expand('%:t')
        endif
      endfunction

    " ....................................................... Error message trap

      " ignore 1st time error messages from plugins (uninitialized s:variables)
      function! core#Quietly(command)
        try
          execute a:command
        catch /.*/
          " discard messages, do nothing
        endtry
      endfunction

    " .............................................................. Debug trace

      let g:trace = $VIMTRACE > '' ? 1 : 0

      function! core#Trace(msg)
        if g:trace == 1
          " escape problematic shell commandline characters
          silent execute '!echo "' . substitute(a:msg, '[-<>#$]', '\\&', 'g') . '" >>/tmp/vim.log'
          " sleep 1000m
        endif
      endfunction

  " Keyboard layout ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Colemak-shift-dh

      " Note: scripts are affected by the mappings below!
      "       e.g. "h" becomes "m", "f" becomes "t" etc.
      "       see thedarnedestthing.com

      " hjkl mapping (0) hjkl (1) mnle
      let s:mnle = "$MNLE" > '' ? $MNLE : 0

      function! core#Colemak()
        if s:mnle != 0
          " map home row (cluster) cursor movement
          nnoremap u     gk
          vnoremap u     gk
          nnoremap n     h
          vnoremap n     h
          nnoremap e     gj
          vnoremap e     gj
          nnoremap i     l
          vnoremap i     l

          " recover vi keys (including caps for consistency)
          nnoremap f     e
          vnoremap f     e
          nnoremap F     E
          vnoremap F     E
          nnoremap h     m
          vnoremap h     m
          nnoremap k     n
          vnoremap k     n
          nnoremap K     N
          vnoremap K     N

          " combine find and till commands
          nnoremap t     f
          vnoremap t     f
          nnoremap T     F
          vnoremap T     F
          nnoremap <A-t> t
          vnoremap <A-t> t
          nnoremap <C-t> T
          vnoremap <C-t> T
        endif
      endfunction

  " GUI ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ............................... Gvim Options (make it look like terminal!)

      " toggle gui menu
      function! core#ToggleGui()
        if &guioptions =~# 'T'
          set guioptions-=T
          set guioptions-=m
        else
          set guioptions+=T
          set guioptions+=m
        endif
      endfunction

    " ................................................................. No tilde

      function! core#NoTilde()
        call core#Trace('core#NoTilde()')
        " hide tilde marker (not applicable to console)
        if $DISPLAY > ''
          execute 'highlight EndOfBuffer guifg=' . g:dfm_bg
          " reset menu highlight after loading autocompletion plugin
          highlight PmenuSel term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=#b58900 guibg=#fdf6e3
          " match command line tab menu
          highlight WildMenu term=reverse ctermfg=0 ctermbg=7 gui=reverse guifg=#b58900 guibg=#fdf6e3
        endif
      endfunction

    " ............................................................. Toggle spell

      function! core#ToggleSpell()
        execute 'let &spell=' . (&spell == 0 ? 1 : 0)
        if PencilMode() != ''
          execute &spell == 0 ? 'NoPencil' : 'Pencil'
        endif
      endfunction

    " ........................................................... Column margins

      " see ui#IndentTheme()
      augroup column
        autocmd!
      augroup END

      " toggle colorcolumn modes
      function! core#ToggleColumn()
        if g:ruler == 0
          let g:ruler      = 1
          let &colorcolumn = col('.')
          autocmd column CursorMoved,CursorMovedI * let &colorcolumn = col('.')
        elseif g:ruler == 1
          let g:ruler      = 2
          autocmd! column
        else
          let g:ruler      = 0
          let &colorcolumn = 0
        endif
        call ui#IndentTheme()
      endfunction

    " ............................................................. Line numbers

      " toggle relative number, line number and no numbering
      function! core#ToggleNumber()
        if (&relativenumber == 1 && &number == 1)
          set norelativenumber
        elseif (&relativenumber == 0 && &number == 1)
          set nonumber
        else
          set relativenumber
          set number
        endif
      endfunction

    " .......................................... White space / soft wrap markers

      " soft wrap marker
      function! core#Soft()
        " filetype dependent textwidth
        if exists('s:soft')
          call matchdelete(s:soft)
        endif
        highlight SoftWrap cterm=underline gui=underline
        let s:soft = '\%' . (&textwidth + 1) . 'v'
        let s:soft = matchadd('SoftWrap', s:soft)
      endfunction

      augroup soft
        autocmd!
      augroup END

      " toggle trailing whitespace highlight and indentation levels
      function! core#ToggleSpaces()
        set list!
        if &list == 0
          match ExtraWhitespace /\%x00$/    " nolist by failing match with null character :)
          call matchdelete(s:soft)
          unlet s:soft
          autocmd! soft
          " echo ''
        else
          match ExtraWhitespace /\s\+$/
          call core#Soft()
          autocmd soft BufEnter * call core#Soft()
          " echo 'List invisibles ON'
        endif
      endfunction

  " Buffer ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ........................................................ Close diff buffer

      " delete any new diff buffer, see buffer.vim DiffOrig
      function! core#CloseDiffOrig()
        wincmd h
        if expand('%') == ''
          bdelete!
          " restore pre-diff settings or subsequent DiffOrig will be *off*
          diffoff
          return 1
        endif
        return 0
      endfunction

    " ............................................................. Buffer count

      function! core#BufCount()
        return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
      endfunction

    " ................................................................ Line wrap

      function! core#ToggleWrap()
        if &formatoptions =~ 't'
          NoPencil
          let &formatoptions = g:codeoptions
          echo 'Automatic line wrap OFF'
        elseif &formatoptions == g:codeoptions
          Pencil
          set formatoptions=tqwan1
          echo 'Automatic line wrap ON'
        else
          set formatoptions
        endif
      endfunction

    " ......................................................... Strip whitespace

      " see https://dougblack.io/words/a-good-vimrc.html
      " strips trailing whitespace from all lines
      function! core#StripTrailingWhitespaces()
        if &modifiable == 1 && ! core#Markdown()
          " save last search & cursor position
          " let l:_s = @/
          " let l:l  = line(".")
          " let l:c  = col(".")
          let s:view = winsaveview()
          %s/\s\+$//e                       " EOL
          %s/\(\n\r\?\)\+\%$//e             " EOF
          call winrestview(s:view)
          " let @/ = l:_s
          " call cursor(l:l, l:c)
        endif
      endfunction

  " Editing ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .......................................................... Shift up / down

      " see https://github.com/gorkunov/vimconfig.git
      function! core#MoveLineUp()
        call core#MoveLineOrVisualUp('.', '')
      endfunction

      function! core#MoveLineDown()
        call core#MoveLineOrVisualDown('.', '')
      endfunction

      function! core#MoveVisualUp()
        call core#MoveLineOrVisualUp("'<", "'<,'>")
        normal! gv
      endfunction

      function! core#MoveVisualDown()
        call core#MoveLineOrVisualDown("'>", "'<,'>")
        normal! gv
      endfunction

      function! core#MoveLineOrVisualUp(line_getter, range)
        let l:line = line(a:line_getter)
        if l:line - v:count1 - 1 < 0
          let l:move = '0'
        else
          let l:move = a:line_getter . ' -' . (v:count1 + 1)
        endif
        call core#MoveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
      endfunction

      function! core#MoveLineOrVisualDown(line_getter, range)
        let l:line = line(a:line_getter)
        if l:line + v:count1 > line('$')
          let l:move = '$'
        else
          let l:move = a:line_getter . ' +' . v:count1
        endif
        call core#MoveLineOrVisualUpOrDown(a:range . 'move ' . l:move)
      endfunction

      function! core#MoveLineOrVisualUpOrDown(move)
        let l:col = virtcol('.')
        execute 'silent! ' . a:move
        execute 'normal! ' . l:col . '|'
      endfunction

    " ......................................................... Insert line wrap

      " insert line while disabling auto-commenting
      function! core#InsertWrap()
        let l:formatoptions = &formatoptions
        set formatoptions-=c
        set formatoptions-=r
        set formatoptions-=o
        normal! ^
        let l:pos = col('.')
        normal! o
        " align line indentation
        execute 'normal! a' . repeat(' ', l:pos)
        let &formatoptions = l:formatoptions
      endfunction

  " Text ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Select text

      function! core#ParagraphAbove()
        if matchstr(getline(line('.')), '\S') == ''
          normal! {
          if matchstr(getline(line('.')), '\S') == ''
            normal! j
          endif
        endif
        normal! }lV{
      endfunction

      function! core#ParagraphBelow()
        if matchstr(getline(line('.')), '\S') == ''
          normal! }
          if matchstr(getline(line('.')), '\S') == ''
            normal! k
          endif
        endif
        normal! {nV}
      endfunction

    " .......................................................... Code block text

      " convert wiki text lines into code block lines
      function! core#CodeBlock()
        execute "silent! normal  :s/\\(.*\\)/`\\1`/\<CR>"
        " preserve leading spaces with wiki markdown
        execute "silent! normal! gv:s/^` /`^ /\<CR>"
        execute "silent! normal! gv:s/^``/`^ `/e\<CR>"
        " convert [[test]], see thedarnedestthing markdown
        execute "silent! normal! gv:s/ \\[\\[ / [[] /e\<CR>"
        execute "silent! normal! gv:s/ \\]\\] / []] /e\<CR>"
      endfunction

    " .......................................................... CSS block align

      " just position cursor on a line with an opening '{'
      function! core#CssBlockAlign()
        " calculate indent width to '.* { '
        let l:indent = repeat(' ', len(substitute(getline(line('.')), '[{].*', '  ', '')))
        let l:start = line('.') + 1
        " end of block
        normal! }
        let l:end   = line('.') - 1
        execute ':' . l:start . ',' . l:end . 's/^ */' . l:indent . '/'
        " next block candidate
        let @/='{.*[^}] *$'
        normal! n
      endfunction

  " Filetype ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " ......................................................... Prose filestypes

      " distraction free filetyes
      function! core#Prose()
        return &filetype =~ 'wiki\|mail\|markdown\|draft\|note'
      endfunction

      function! core#Markdown()
        return &filetype =~ 'wiki\|markdown'
      endfunction

    " ................................................................... Tagbar

      function! core#Tagbar()
        return &filetype == 'tagbar'
      endfunction

    " ................................................................ Protected

      function! core#Protected()
        return &filetype == 'help' || mode() == 't'
      endfunction

    " ............................................................... Modifiable

      " [regex name, filetype, modifiable, wordcount] rule tuple
      " modifiable (0) nomodifiable (1) modifiable
      " wordcount (0) no word count (1) statusline with wordcount
      " note "/name" to represent "^name"
      let s:nametypes =
          \[
          \  ['conf$',          'conf',     1, 0]
          \, ['config$',        'conf',     1, 0]
          \, ['draft$',         'markdown', 1, 1]
          \, ['eml$',           'mail',     1, 1]
          \, ['error$',         'log',      0, 0]
          \, ['log$',           'log',      0, 0]
          \, ['note$',          'markdown', 1, 1]
          \, ['rc$',            'rc',       1, 0]
          \, ['txt$',           'text',     0, 1]
          \, ['wiki$',          'markdown', 1, 1]
          \, ['\(^\|/\)readme', 'text',     0, 1]
          \]

      " [regex fileinfo, filetype, modifiable, readonly] rule tuple
      " modifiable (0) nomodifiable (1) modifiable
      " readonly (0) not set (1) set
      let s:contenttypes =
          \[
          \  ['binary',             'binary', 0, 1]
          \, ['no read permission', 'binary', 0, 1]
          \, ['text',               'text',   1, 0]
          \]

      " set buffer attributes by known filetypes
      function! core#CheckFiletype()
        for [name, filetype, modifiable, wordcount] in s:nametypes
          if expand('%') =~ name
            let &filetype   = (&filetype == '' ? filetype : &filetype)
            let &modifiable = modifiable
            break
          endif
        endfor
        if &filetype == ''                  " by file content if not autodetected
          for [content, filetype, modifiable, readonly] in s:contenttypes
            if system('file -i ' . expand('%') . '|cut -d: -f2') =~ content
              let &filetype   = filetype
              let &modifiable = modifiable
              let &readonly   = readonly
            endif
          endfor
        endif
        " see Snipmate plugins.vim
        if &filetype == ''
          let &filetype = 'new'
        endif
      endfunction

    " ................................................................... E-mail

      function! core#ComposeMail()
        " email has blank lines inserted externally (via sed) for replys to
        " avoid the previously messy and unpredictable editing mode vim commands
        " see dmenu compose
        call ui#FontSize(1)
        " gg/.. cannot be combined into single expression (produces unpredictable results)
        execute 'normal! gg'
        " execute 'normal! ' . search('\n\n\n', 'e') . 'G'
        execute 'normal! ' . (search('^Subject: ') + 2) . 'G'
        execute 'startinsert'
      endfunction

" core.vim
