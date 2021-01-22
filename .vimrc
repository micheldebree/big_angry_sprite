au FileType asm set ft=kickass
set makeprg=make\ %<.prg
noremap <F6> :wa<CR>:silent! make <bar> cwindow<CR>:redraw!<CR>
noremap <F7> :wa<CR>:make %<.debug<bar> cwindow<CR>:redraw!<CR>
noremap <F8> :wa<CR>:make %<.exe.prg<bar> cwindow<CR>:redraw!<CR>
set errorformat=%EError:\ %m,%Cat\ line\ %l\\,\ column\ %c\ in\ %f,%Z
set autoindent
set shiftwidth=2
set tabstop=2
set softtabstop=2
set smartindent
set expandtab
set foldmethod=marker
set foldlevel=0
au FileType asm set commentstring=//%s
au BufWinLeave mkview
au BufWinEnter silent! loadview
