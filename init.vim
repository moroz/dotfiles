call plug#begin('~/.config/nvim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'vim-utils/vim-ruby-fold'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-endwise'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'ap/vim-css-color', { 'for' : ['sass', 'css', 'scss'] }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'flazz/vim-colorschemes'
Plug 'xolox/vim-misc'
Plug 'AndrewRadev/switch.vim', { 'for' : 'ruby' }
Plug 'xolox/vim-easytags'
Plug 'easymotion/vim-easymotion'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'cakebaker/scss-syntax.vim', { 'for': 'sass' }
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'
Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
Plug 'nathanaelkane/vim-indent-guides'
Plug 'neomake/neomake'
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

autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType yaml set inde=
autocmd Filetype tex command! Tex Dispatch! xelatex %
au BufRead,BufNewFile all set wrap linebreak nolist textwidth=0 wrapmargin=0
map <Leader> <Plug>(easymotion-prefix)
" Fix indentation on entire file
map <Leader>ri mzgg=G`z

call neomake#configure#automake({
  \ 'TextChanged': {},
  \ 'InsertLeave': {},
  \ 'BufWritePost': {'delay': 0},
  \ 'BufWinEnter': {},
  \ }, 500)

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

let NERDTreeMinimalUI=25
let NERDTreeDirArrows=1

nnoremap <C-Down> :m .+1<CR>==
nnoremap <C-Up> :m .-2<CR>==
inoremap <C-Down> <Esc>:m .+1<CR>==gi
inoremap <C-Up> <Esc>:m .-2<CR>==gi
vnoremap <C-Down> :m '>+1<CR>gv=gv
vnoremap <C-Up> :m '<-2<CR>gv=gvn
map <Leader>ev :tabedit $MYVIMRC<CR>

nnoremap <C-j> <c-w>j
nnoremap <C-h> <c-w>h
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
nnoremap <C-t> :tabe<CR>
nnoremap j gj
nnoremap k gk
nnoremap <F5> :Buffers<CR>
nnoremap <F4> :GundoToggle<CR>
map <F9> :NERDTreeToggle<CR>
"map <F9> :Lexplore<CR>
nnoremap <F6> :%y +<CR>
nnoremap <C-p> :Files<CR>
nnoremap <F7> :Colors<cr>
nnoremap <F3> :Tags<CR>
inoremap <D-s> <esc>:w<cr>
nnoremap <D-s> :w<cr>
inoremap <C-s> <esc>:update<cr>
nnoremap <C-s> :update<cr>
nnoremap <Tab> gt
nnoremap <S-Tab> gT
map <F10> :bufdo update<CR>:bufdo q<CR>
map <silent> <Esc><Esc> :noh<CR>

nnoremap <space>ga :Git add %:p<CR><CR>
nnoremap <space>gs :Gstatus<CR>
nnoremap <space>gl :Git add .<CR><CR>
nnoremap <space>gc :Gcommit -v -q<CR>
nnoremap <space>gt :Gcommit -v -q %:p<CR>
nnoremap <space>gd :Gdiff<CR>

if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor\ --column
    set grepformat=%f:%l:%c%m
endif

autocmd FileType netrw set nolist

set tags=./tags
let g:easytags_dynamic_files = 1
let g:easytags_async = 1
let g:easytags_events = ['BufWritePost']
let g:jsx_ext_required = 0
let g:airline_powerline_fonts = 1

colorscheme distinguished
"set bg=dark
if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    let g:python_host_prog = '/usr/local/bin/python2'
    let g:python3_host_prog = '/usr/local/bin/python3'
  endif
endif

function! NumberToggle()
  if(&relativenumber == 1)
    set nornu
  else
    set relativenumber
  endif
endfunc

let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=bg
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=black

nnoremap <C-n> :call NumberToggle()<cr>
set mouse=a
set guicursor=
