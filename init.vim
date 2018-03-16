call plug#begin('~/.config/nvim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'flazz/vim-colorschemes'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'cakebaker/scss-syntax.vim', { 'for': 'sass' }
Plug 'tpope/vim-rails'
Plug 'tpope/vim-endwise'
Plug 'ctjhoa/spacevim'
Plug 'tpope/vim-surround'
Plug 'neomake/neomake'
Plug 'vim-scripts/fcitx.vim'
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

let mapleader = " "
nnoremap <Space> <Nop>
autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType yaml set inde=
autocmd Filetype tex command! Tex Dispatch! xelatex %
au BufRead,BufNewFile all set wrap linebreak nolist textwidth=0 wrapmargin=0
let NERDTreeMinimalUI=28
let NERDTreeDirArrows=1
"if strftime("%H%M") < 1430 && strftime("%H") > 5
"  colorscheme hemisu
"else
  colorscheme Tomorrow-Night-Bright
  let g:airline_theme='angr'
"endif

call neomake#configure#automake({
  \ 'BufWritePost': {'delay': 500}})
let g:neomake_ruby_enabled_makers = ['mri', 'rubocop']
let g:neomake_warning_sign = {
  \ 'text': '>>',
  \ 'texthl': 'WarningMsg',
  \ }

let g:neomake_error_sign = {
  \ 'text': '>>',
  \ 'texthl': 'ErrorMsg',
  \ }

let g:neomake_info_sign = {
  \ 'text': '>>',
  \ 'texthl': 'NeomakeInfoSign'
  \ }

nnoremap <C-c> "+yy
nnoremap <C-t> :tabe<CR>
nnoremap j gj
nnoremap k gk
map <F7> :Colors<CR>
map <F9> :NERDTreeToggle<CR>
inoremap <C-s> <esc>:update<cr>
nnoremap <C-s> :update<cr>
nnoremap <C-p> :Files<cr>
nnoremap <Tab> gt
nnoremap <S-Tab> gT
map <F10> :bufdo update<CR>:bufdo q<CR>
map <silent> <Esc><Esc> :noh<CR>
autocmd FileType netrw set nolist

map <Leader>wd :q<cr>
map <Leader>Ts :Colors<cr>
map <Leader>fer :source $MYVIMRC<cr>
map <Leader>gs :Magit<cr>
map <Leader>pg :Tags<cr>

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
