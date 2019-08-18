" sdothum - 2016 (c) wtfpl

" Defaults
" ══════════════════════════════════════════════════════════════════════════════

  " Mode _______________________________________________________________________

    " .............................................................. Debug trace

      let g:trace = $VIMTRACE > '' ? 1 : 0  " touch ~/.session/vimtrace

      " escape problematic shell commandline characters
      function! s:trace(msg)
        if g:trace == 1 | silent execute '!echo "' . substitute(a:msg, '[-<>#$]', '\\&', 'g') . '" >>/tmp/vim.log' | endif
      endfunction

      command! -nargs=1 Trace call <SID>trace(<f-args>)

    " ................................................................. Terminal

      " !term fails on shell error 1 (?)
      command! Term :call system('term "vimterm" STACK')

  " Registers __________________________________________________________________

    " ................................................................... Macros

      " https://www.reddit.com/r/vim/comments/aqmnaf/handy_shortcut_to_repeat_the_last_recorded_macro/
      function! s:replayLastMacro()
        try
          normal @@
        catch /E748/
          normal @q
        endtry
      endfunction

      command! ReplayLastMacro silent! call <SID>replayLastMacro()

  " Format _____________________________________________________________________

    " ................................................................ Line wrap

      function! s:toggleWrap()
        if &formatoptions =~ 't'
          NoPencil
          let &formatoptions = g:codeoptions
        elseif &formatoptions == g:codeoptions
          Pencil
          set formatoptions=tqwan1
        else
          set formatoptions
        endif
        call Status('Automatic line wrap', &formatoptions =~ 't')
      endfunction

      command! ToggleWrap call <SID>toggleWrap()

  " Search and replace _________________________________________________________

    " ....................................................... Incremental search

      function! s:toggleWrapSearch()
        let g:separator = g:separator == ' ' ? '\_s*' : ' '
        cnoremap <expr><space>  '/?' =~ getcmdtype() ? g:separator : ' '
        call Status('Wrap search', g:separator != ' ')
      endfunction

      command! ToggleWrapSearch call <SID>toggleWrapSearch()

    " .................................................................. Replace

      " restore search highlight after replace
      function! s:searchReplace(cmd)
        let l:search = @/
        let l:s = input('', a:cmd)
        execute l:s
        let @/ = l:search
      endfunction

      command! -nargs=1 SearchReplace silent! call <SID>searchReplace(<f-args>)

" default.vim
