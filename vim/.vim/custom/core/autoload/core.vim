" sdothum - 2016 (c) wtfpl

" Primitive
" ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

  " System ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

    " .............................................................. Reload vim

      " this function can only be defined in autoloaded source to avoid reload conflict
      function! core#Vimrc()
        execute 'wall'
        autocmd!
        source $MYVIMRC
        call theme#LiteSwitch()
        call theme#LiteSwitch()
        RedrawGui                           " see gui.vim
      endfunction

    " ............................................................. Buffer count

      function! core#BufCount()
        return len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
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

    " .............................................................. Auto backup

      " queue files written for vhg (may contain repeated update entries)
      function! core#QueueFile()
        " see v script (sets QUEUE and invokes vhg)
        let l:path = resolve(expand('%:p'))
        if l:path =~ g:repo && $QUEUE > ''
          let l:file = substitute(l:path, g:repo, '', '')
          let l:cmd  = 'echo ' . l:file . ' >>' . $HOME . '/.vim/job/' . $QUEUE
          call system(l:cmd)
        endif
      endfunction

      " :wall on FocusLost does not trigger autocmd BufWrite (?), see buffers.vim
      function! core#QueueBuffers()
        set lazyredraw
        let l:cur_buffer = bufnr('%')
        for l:buf in getbufinfo({'buflisted':1})
          if l:buf.changed
            execute 'buffer' . l:buf.bufnr
            update
            call core#QueueFile()
          endif
        endfor
        execute 'buffer' . l:cur_buffer
        set nolazyredraw
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

    " ........................................................... Column margins

      augroup column
        autocmd!
      augroup END

      " highlight wrapped line portion, see theme#Theme()
      function! core#ColumnWrap()
        if g:ruler == 0 && g:wraplight
          let l:edge       = winwidth(0) - &numberwidth - &foldcolumn - 1
          let &colorcolumn = join(range(l:edge, 999), ',')
        else
        endif
      endfunction

      function! core#ToggleColumnWrap(...)
        let g:wraplight = a:0 ? a:1 : (g:wraplight ? 0 : 1)
        let g:ruler     = -1
        call core#ToggleColumn()
      endfunction

      " toggle colorcolumn modes, see theme#IndentTheme()
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
          call core#ColumnWrap()
        endif
        call theme#IndentTheme()
        " flash column position, see autocmd info.vim
        let g:column = 1
      endfunction

  " Text ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

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
        return &filetype =~ 'wiki\|mail\|markdown\|draft\|note\|html'
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

    " ........................................................... Plugin windows

      " plugin buffers typically are named '[<plugin>]' or '__<plugin>__'
      function! core#PluginWindow()
        return expand('%:r') =~ '^[[_].*'
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
        call theme#FontSize(1)
        " gg/.. cannot be combined into single expression (produces unpredictable results)
        execute 'normal! gg'
        " execute 'normal! ' . search('\n\n\n', 'e') . 'G'
        execute 'normal! ' . (search('^Subject: ') + 2) . 'G'
        execute 'startinsert'
      endfunction

" core.vim
