" sdothum - 2016 (c) wtfpl

" Buffers
" ══════════════════════════════════════════════════════════════════════════════

" File buffers _________________________________________________________________

" ............................................................ Close diff buffer
" delete any new diff buffer
function! s:closeDiff()
  if &diff  " caution: wincmd resets active window (affects :Buffer)
    wincmd h
    if empty(expand('%'))
      bdelete!
      diffoff  " restore pre-diff settings or subsequent OpenDiff will be *off*
      return 1
    endif
  endif
  return 0
endfunction

command! CloseDiff silent! call <SID>closeDiff()

" .................................................................... Open diff
" toggle diff of current file
command! OpenDiff if !<SID>closeDiff() | vert new | set bt=nofile | r ++edit # | 0d_
                    \| diffthis | wincmd p | diffthis | endif

" File actions _________________________________________________________________

" .......................................................... Buffer close / save
" close all other buffers (and newly created no name buffer)
command! Singleton   CloseDiff | %bdelete | edit # | bdelete #
" close OpenDiff or current buffer
command! CloseUnique if !<SID>closeDiff() | silent bdelete! | endif

" .................................................................. Auto backup
" queue files written for vhg (may contain repeated update entries)
function! s:queueFile()
  let l:path = resolve(expand('%:p'))  " see v script (sets QUEUE and invokes vhg)
  if l:path =~ s:repo && !empty($QUEUE)
    let l:file = substitute(l:path, s:repo, '', '')
    let l:cmd  = 'echo ' . l:file . ' >>' . $HOME . '/.vim/job/' . $QUEUE
    call system(l:cmd)
  endif
endfunction

command! QueueFile silent! call <SID>queueFile()

" :wall on FocusLost does not trigger autocmd BufWrite (?)
function! s:queueBuffers()
  if CommandWindow() | return | endif
  set lazyredraw
  let l:cur_buffer = bufnr('%')
  for l:buf in getbufinfo({'buflisted':1})
    if l:buf.changed
      execute 'buffer' . l:buf.bufnr
      update
      QueueFile
    endif
  endfor
  execute 'buffer' . l:cur_buffer
  set nolazyredraw
endfunction

command! QueueBuffers silent! call <SID>queueBuffers()

" Buffer handling ______________________________________________________________

" ................................................................ Switch buffer
" nmap <silent><Enter> :CloseDiff<CR>:silent bnext<CR>:call theme:SplitColors()<CR>
function! s:enter()
  if CommandWindow()  " on q: to enter command-line window
    execute "normal! \<CR>"
  else
    CloseDiff
    silent bnext
    SplitColors
  endif
endfunction

command! Enter silent! call <SID>enter()

" buffer.vim
