" sdothum - 2016 (c) wtfpl

" Primitive
" ══════════════════════════════════════════════════════════════════════════════

  " System _____________________________________________________________________

    " .............................................................. Reload vim

      " this function can only be defined in autoloaded source to avoid reload conflict
      function! core#Vimrc()
        execute 'wall'
        autocmd!
        source $MYVIMRC
        call theme#LiteSwitch()
        call theme#LiteSwitch()
        RedrawGui
      endfunction

    " ....................................................... Error message trap

      " ignore 1st time error messages from plugins (uninitialized s:variables)
      function! core#Quietly(command)
        try
          execute a:command
        catch /.*/  " discard messages
        endtry
      endfunction

    " .................................................... Current state message
    
      function! core#Status(message, state)
        echo a:message . (a:state ? ' ON' : ' OFF')
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
        echo 'Printing..'
        if s:markdown()                    | execute '!hardcopy wiki \"' . expand('%:t') . '\"'
        elseif expand('%:p') =~ 'Patricia' | execute '!hardcopy wps' expand('%:t')
        else                               | execute '!hardcopy code' expand('%:t') | endif
      endfunction

    " .............................................................. Debug trace

      let g:trace = $VIMTRACE > '' ? 1 : 0

      " escape problematic shell commandline characters
      function! core#Trace(msg)
        if g:trace == 1 | silent execute '!echo "' . substitute(a:msg, '[-<>#$]', '\\&', 'g') . '" >>/tmp/vim.log' | endif
      endfunction

  " Text _______________________________________________________________________

    " ........................................................... Non-blank line

      function! core#NonblankLine()
        return matchstr(getline(line('.')), '\S') > ''
      endfunction

      function! core#BlankLine()
        return ! core#NonblankLine()
      endfunction

    " ......................................................... Strip whitespace

      " strips trailing whitespace from all lines, see https://dougblack.io/words/a-good-vimrc.html
      function! core#StripTrailingWhitespaces()
        if &modifiable == 1 && ! s:markdown()
          " let l:_s = @/ " save last search & cursor position
          " let l:l  = line(".")
          " let l:c  = col(".")
          let s:view = winsaveview()
          %s/\s\+$//e           " EOL
          %s/\(\n\r\?\)\+\%$//e " EOF
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
        let l:indent = repeat(' ', len(substitute(getline(line('.')), '[{].*', '  ', ''))) " calculate indent width to '.* { '
        let l:start = line('.') + 1
        normal! }
        let l:end   = line('.') - 1
        execute ':' . l:start . ',' . l:end . 's/^ */' . l:indent . '/'
        let @/ = '{.*[^}] *$' " next block candidate
        normal! n
      endfunction

  " Filetype ___________________________________________________________________

    " ......................................................... Prose filestypes

      " distraction free filetyes
      function! core#Prose()
        return &filetype =~ 'wiki\|mail\|markdown\|draft\|note\|html'
      endfunction

      function! s:markdown()
        return &filetype =~ 'wiki\|markdown'
      endfunction

    " ................................................................ Protected

      function! core#Protected()
        return &filetype == 'help' || mode() == 't' || g:fzf#vim#buffers != {} " fzf trap
      endfunction

    " ........................................................... Plugin windows

      " plugin buffers typically are named '[<plugin>]' or '__<plugin>__'
      function! core#PluginWindow()
        return expand('%:r') =~ '^[[_].*'
      endfunction

      function! core#CommandWindow()
        return expand('%p') == '[Command Line]'
      endfunction

    " ............................................................... Modifiable

      " [regex name, filetype, modifiable, wordcount], "/name" to represent "^name"
      " modifiable (0) nomodifiable (1) modifiable
      " wordcount (0) no word count (1) statusline with wordcount
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

      " [regex fileinfo, filetype, modifiable, readonly]
      " modifiable (0) nomodifiable (1) modifiable
      " readonly (0) not set (1) set
      let s:contenttypes =
          \[
          \  ['binary',             'binary', 0, 1]
          \, ['no read permission', 'binary', 0, 1]
          \, ['text',               'conf',   1, 0]
          \]

      " set buffer attributes by known filetypes
      function! core#BufferSettings()
        for [name, filetype, modifiable, wordcount] in s:nametypes
          if expand('%') =~ name
            let &filetype   = (&filetype == '' ? filetype : &filetype)
            let &modifiable = modifiable
            break
          endif
        endfor
        if &filetype == '' " by file content if not autodetected
          for [content, filetype, modifiable, readonly] in s:contenttypes
            if system('file -i ' . expand('%') . '|cut -d: -f2') =~ content
              let &filetype   = filetype
              let &modifiable = modifiable
              let &readonly   = readonly
            endif
          endfor
        endif
        if &filetype == '' | let &filetype = 'new' | endif " see Snipmate plugins.vim
      endfunction

    " ................................................................... E-mail

      " email has blank lines inserted externally (via sed) for replys, see dmenu compose
      function! core#ComposeMail()
        execute 'normal! gg'
        execute 'normal! ' . (search('^\(\(Subject\|From\|To\|Cc\):.*\n\(Subject\|From\|To\|Cc\):.*\n\(Subject\|From\|To\|Cc\):.*\n\)') + 4) . 'G'
        execute 'startinsert'
      endfunction

" core.vim
