if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')
" Project management
Plug 'scrooloose/nerdtree', { 'on':  ['NERDTreeToggle','NERDTreeFind'] }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'thaerkh/vim-workspace'
Plug 'tpope/vim-fugitive'
Plug 'ludovicchabant/vim-gutentags'

" Editing
Plug 'easymotion/vim-easymotion'
Plug 'scrooloose/nerdcommenter'
Plug 'ctjhoa/spacevim'
Plug 'tpope/vim-surround'
Plug 'neomake/neomake'
Plug 'tpope/vim-endwise'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }

" Ruby
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }

" Elixir
Plug 'elixir-editors/vim-elixir', { 'for': ['elixir', 'eelixir'] }

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'mhinz/vim-mix-format', { 'for': 'elixir' }
Plug 'vim-scripts/fcitx.vim', { 'for': 'tex' }
Plug 'jacoborus/tender.vim'
Plug 'chriskempson/base16-vim'

" Javascript & React
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'jsx'] }
Plug 'mxw/vim-jsx', { 'for': ['javascript', 'jsx'] }
Plug 'mattn/emmet-vim', { 'for': ['javascript', 'html', 'eelixir'] }
Plug 'cakebaker/scss-syntax.vim', { 'for': 'sass' }
Plug 'jparise/vim-graphql', { 'for': ['javascript', 'jsx', 'graphql'] }
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
Plug 'Quramy/tsuquyomi', { 'for': 'typescript' }
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
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
set autoread

let mapleader = " "
nnoremap <Space> <Nop>
autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType html,eelixir,javascript,jsx EmmetInstall
autocmd FileType yaml set inde=
autocmd Filetype tex command! Tex Dispatch! xelatex %
au BufRead,BufNewFile all set wrap linebreak nolist textwidth=0 wrapmargin=0
let NERDTreeMinimalUI=28
let NERDTreeDirArrows=1
let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-material-darker

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

let g:neomake_elixir_enabled_makers = []

let g:deoplete#enable_at_startup = 1
" Disable the candidates in Comment/String syntaxes.
call deoplete#custom#source('_',
            \ 'disabled_syntaxes', ['Comment', 'String'])

nnoremap <C-c> "+yy
nnoremap <C-t> :tabe<CR>
nnoremap j gj
nnoremap k gk
inoremap <C-s> <esc>:update<cr>
nnoremap <C-s> :update<cr>
nnoremap <C-p> :Files<cr>
nnoremap <Tab> gt
nnoremap <S-Tab> gT
map <silent> <Esc><Esc> :noh<CR>
autocmd FileType netrw set nolist
map <F4> :GundoToggle<cr>
map <F8> :NERDTreeFind<cr>
map <F9> :NERDTreeToggle<cr>
map <F10> :wqa<CR>
" Select all occurrences of selected text
vnoremap // y/\V<C-R>"<CR>

map <Leader>h <C-w><C-h>
map <Leader>l <C-w><C-l>

map <Leader>wd :q<cr>
map <Leader>Ts :Colors<cr>
map <Leader>fer :source $MYVIMRC<cr>
map <Leader>pi :PlugInstall<cr>
map <Leader>pg :Tags<cr>

map <Leader>gs :Gstatus<cr>
map <Leader>ga :Git add .<cr>
map <Leader>gc :Gcommit<cr>
map <Leader>ds :ToggleWorkspace<cr>

let g:jsx_ext_required = 0
let g:airline_powerline_fonts = 1
let g:mix_format_on_save = 1
let g:workspace_session_disable_on_args = 1
let g:workspace_autosave = 0
let g:NERDSpaceDelims = 1
let g:gutentags_file_list_command='ag -l --ignore spec/ --ignore public/'

let g:user_emmet_leader_key='<Tab>'
let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}
let g:user_emmet_install_global = 0
let NERDTreeQuitOnOpen = 0

if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    let g:python_host_prog = '/usr/local/bin/python2'
    let g:python3_host_prog = '/usr/local/bin/python3'
  endif
endif

if (has("termguicolors"))
 set termguicolors
endif

set mouse=a
