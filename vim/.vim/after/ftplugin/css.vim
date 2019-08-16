" sdothum - 2016 (c) wtfpl

" ftplugin
" ══════════════════════════════════════════════════════════════════════════════

    " ...................................................................... CSS
  
      set nospell expandtab tabstop=2 shiftwidth=2 softtabstop=2
      
      " sort align, note <C-v><keycode> to embed command mode keycode
      nmap <silent><leader>css :g/{/normal! f{viB:sort<C-v><CR><CR>
      nmap <silent><leader>cSS :g/{/normal! f{viB:EasyAlign<C-v><CR><C-v><Space><CR>
      nmap <silent><leader>CSS :g/{/normal! f{viB:sort<C-v><CR><CR>:g/{/normal! f{viB:EasyAlign<C-v><CR><C-v><Space><CR>

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

      nnoremap <silent><S-F4> silent! call <SID>cssBlockAlign()

" css.vim

