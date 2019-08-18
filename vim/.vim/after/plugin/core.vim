" sdothum - 2016 (c) wtfpl

" Core
" ══════════════════════════════════════════════════════════════════════════════

  " Vim ________________________________________________________________________

    " ............................................................... Reload vim

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

  " System _____________________________________________________________________

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

  " Text _______________________________________________________________________

    " ......................................................... (Non-)blank line

      function! NonBlankLine()
        return matchstr(getline(line('.')), '\S') > ''
      endfunction

      function! BlankLine()
        return ! NonBlankLine()
      endfunction

    " ............................................................... Print file

      " latex printing
      function! s:hardcopy()
        echo 'Printing..'
        if s:markdown()                    | execute '!hardcopy wiki \"' . expand('%:t') . '\"'
        elseif expand('%:p') =~ 'Patricia' | execute '!hardcopy wps' expand('%:t')
        else                               | execute '!hardcopy code' expand('%:t') | endif
      endfunction

      command! Hardcopy silent call <SID>hardcopy()

  " Filetype ___________________________________________________________________

    " ......................................................... Prose filestypes

      " distraction free filetyes
      function! Prose()
        return &filetype =~ 'wiki\|mail\|markdown\|draft\|note\|html'
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

" core.vim
