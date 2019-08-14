" sdothum - 2016 (c) wtfpl

" Primitive
" ══════════════════════════════════════════════════════════════════════════════

  " System _____________________________________________________________________

    " .............................................................. Reload vim

      " this function can only be defined in autoloaded source to avoid reload conflict
      function! s:vimrc()
        execute 'wall'
        autocmd!
        source $MYVIMRC
        LiteSwitch
        LiteSwitch
        RedrawGui
      endfunction

      " when updates won't break the current vim session!
      command! Vimrc call <SID>vimrc()

    " .............................................................. Config file

      " handy searchable lists
      command! Hi  enew | put=execute('hi')  | normal gg
      command! Map enew | put=execute('map') | normal gg

    " ....................................................... Error message trap

      " ignore 1st time error messages from plugins (uninitialized s:variables)
      function! s:quietly(command)
        try
          execute a:command
        catch /.*/  " discard messages
        endtry
      endfunction

      command! -nargs=1 Quietly call <SID>quietly(<f-args>)

    " .................................................... Current state message
    
      function! Status(message, state)
        echo a:message . (a:state ? ' ON' : ' OFF')
      endfunction

    " ............................................................ Open terminal

      " !term fails on shell error 1 (?)
      command! Term :call system('term "vimterm" STACK')

    " ............................................................... Print file

      " latex printing
      function! s:hardcopy()
        echo 'Printing..'
        if s:markdown()                    | execute '!hardcopy wiki \"' . expand('%:t') . '\"'
        elseif expand('%:p') =~ 'Patricia' | execute '!hardcopy wps' expand('%:t')
        else                               | execute '!hardcopy code' expand('%:t') | endif
      endfunction

      command! Hardcopy silent call <SID>hardcopy()

    " .............................................................. Debug trace

      let g:trace = $VIMTRACE > '' ? 1 : 0

      " escape problematic shell commandline characters
      function! s:trace(msg)
        if g:trace == 1 | silent execute '!echo "' . substitute(a:msg, '[-<>#$]', '\\&', 'g') . '" >>/tmp/vim.log' | endif
      endfunction

      command! -nargs=1 Trace call <SID>trace(<f-args>)

  " Text _______________________________________________________________________

    " ........................................................... Non-blank line

      function! NonBlankLine()
        return matchstr(getline(line('.')), '\S') > ''
      endfunction

      function! BlankLine()
        return ! NonBlankLine()
      endfunction

    " .......................................................... CSS block align

      " just position cursor on a line with an opening '{'
      function! s:cssBlockAlign()
        let l:indent = repeat(' ', len(substitute(getline(line('.')), '[{].*', '  ', ''))) " calculate indent width to '.* { '
        let l:start = line('.') + 1
        normal! }
        let l:end   = line('.') - 1
        execute ':' . l:start . ',' . l:end . 's/^ */' . l:indent . '/'
        let @/ = '{.*[^}] *$' " next block candidate
        normal! n
      endfunction

      command! CssBlockAlign silent! call <SID>cssBlockAlign()

  " Filetype ___________________________________________________________________

    " ......................................................... Prose filestypes

      " distraction free filetyes
      function! Prose()
        return &filetype =~ 'wiki\|mail\|markdown\|draft\|note\|html' || expand('%:e') == 'wiki'
      endfunction

      function! Markdown()
        return &filetype =~ 'wiki\|markdown'
      endfunction

    " ................................................................ Protected

      function! s:fzfBuffer()
        if exists("g:fzf#vim#buffers")
          return g:fzf#vim#buffers != {} " fzf trap
        else
          return 0
        endif
      endfunction

      function! Protected()
        return &filetype == 'help' || mode() == 't' || <SID>fzfBuffer()
      endfunction

    " ........................................................... Plugin windows

      " plugin buffers typically are named '[<plugin>]' or '__<plugin>__'
      function! PluginWindow()
        return expand('%:r') =~ '^[[_].*'
      endfunction

      function! CommandWindow()
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
      function! s:bufferSettings()
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

      command! BufferSettings silent! call <SID>bufferSettings()

    " ................................................................... E-mail

      " email has blank lines inserted externally (via sed) for replys, see dmenu compose
      function! s:composeMail()
        execute 'normal! gg'
        execute 'normal! ' . (search('^\(\(Subject\|From\|To\|Cc\):.*\n\(Subject\|From\|To\|Cc\):.*\n\(Subject\|From\|To\|Cc\):.*\n\)') + 4) . 'G'
        execute 'startinsert'
      endfunction

      command! ComposeMail silent! call <SID>composeMail()

" core.vim
