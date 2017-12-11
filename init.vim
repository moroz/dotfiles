call plug#begin('~/.config/nvim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'easymotion/vim-easymotion'
Plug 'vim-airline/vim-airline'
call plug#end()

set tabstop=2
set shiftwidth=2
set expandtab
set number
set hidden
set laststatus=2
set showcmd
set noswapfile
set incsearch
set ignorecase
set lbr
set smartindent
set eol

autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType yaml set inde=
autocmd Filetype tex command! Tex Dispatch! xelatex %
au BufRead,BufNewFile all set wrap linebreak nolist textwidth=0 wrapmargin=0
map <Leader> <Plug>(easymotion-prefix)
let NERDTreeMinimalUI=25
let NERDTreeDirArrows=1
map <Leader>ev :tabedit $MYVIMRC<CR>

nnoremap <C-j> <c-w>j
nnoremap <C-h> <c-w>h
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
nnoremap <C-t> :tabe<CR>
nnoremap j gj
nnoremap k gk
map <F9> :NERDTreeToggle<CR>
inoremap <D-s> <esc>:w<cr>
nnoremap <D-s> :w<cr>
inoremap <C-s> <esc>:update<cr>
nnoremap <C-s> :update<cr>
nnoremap <Tab> gt
nnoremap <S-Tab> gT
map <F10> :bufdo update<CR>:bufdo q<CR>
map <silent> <Esc><Esc> :noh<CR>
autocmd FileType netrw set nolist

let g:jsx_ext_required = 0
let g:airline_powerline_fonts = 1

if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    let g:python_host_prog = '/usr/local/bin/python2'
    let g:python3_host_prog = '/usr/local/bin/python3'
  endif
endif

nnoremap <C-n> :call NumberToggle()<cr>
set mouse=a
set guicursor=
