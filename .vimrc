au FileType asm set ft=kickass
noremap <F6> :wa<CR>:make %<.prg<bar> cwindow<CR>:redraw!<CR>
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
au FileType asm setlocal guifont=unscii-16-full:h16,C64\ Pro\ Mono:h12
