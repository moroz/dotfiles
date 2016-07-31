execute pathogen#infect()
syntax on
filetype plugin indent on

set tabstop=2
set shiftwidth=2
set expandtab

python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

set rtp+=$HOME/.local/lib/python2.7/site-packages/powerline/bindings/vim/

" Always show statusline
set laststatus=2
"
" " Use 256 colours (Use this setting only if your terminal supports 256
" colours)
set t_Co=256

let NERDTreeMinimalUI=1
let NERDTreeDirArrows=1

let g:ruby_path="/usr/bin/env ruby"
set nocompatible
set backspace=2
set history=500
set ruler
set autowrite
set showcmd
set number
set incsearch
set switchbuf=usetab
set lines=40 columns=169
set hidden

nnoremap <C-Tab> :bn<CR>
nnoremap <C-S-Tab> :bp<CR>
nnoremap <C-Up> :m .-2<CR>==
nnoremap <C-Down> :m +1<CR>==

map <F10> :NERDTreeToggle<CR>
map <F9> :NERDTreeFind<CR>
set updatetime=300
