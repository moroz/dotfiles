call plug#begin('~/.config/nvim/plugged')
" Plugins will go here in the middle.
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'rking/ag.vim'
Plug 'tpope/vim-endwise'
Plug 'airblade/vim-gitgutter'
call plug#end()

set tabstop=2
set shiftwidth=2
set expandtab
autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags

let NERDTreeMinimalUI=25
let NERDTreeDirArrows=1

nnoremap <C-Up> :m .-2<CR>==
nnoremap <C-Down> :m +1<CR>==
nnoremap <C-Left> <c-w>h
nnoremap <C-Right> <c-w>l
nnoremap <C-j> <c-w>j
nnoremap <C-h> <c-w>h
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
nnoremap <F5> :buffers<CR>:buffer<Space>
map <F9> :NERDTreeToggle<CR>
nnoremap <F6> :%y +<CR>
map <F7> :%s ///g
map <F2> :w<CR>
inoremap <F2> <Esc>:w<CR>
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>
map <F10> :bufdo update<CR>:bufdo q<CR>

if executable('ag')
    " Note we extract the column as well as the file and line number
    set grepprg=ag\ --nogroup\ --nocolor\ --column
    set grepformat=%f:%l:%c%m
endif
