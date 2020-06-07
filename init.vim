if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')
" Project management
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'preservim/nerdtree', { 'on':  ['NERDTreeToggle','NERDTreeFind'] }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-dispatch'
Plug 'digitaltoad/vim-pug', { 'for': 'pug' }

" Editing
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-commentary'
Plug 'ctjhoa/spacevim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'airblade/vim-gitgutter'
Plug 'slim-template/vim-slim'
Plug 'jiangmiao/auto-pairs'

" Elixir
Plug 'elixir-editors/vim-elixir'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'mhinz/vim-mix-format', { 'for': 'elixir' }
" Plug 'vim-scripts/fcitx.vim' ", { 'for': ['tex', 'text', 'markdown'] }
Plug 'chriskempson/base16-vim'
Plug 'flazz/vim-colorschemes'
Plug 'sonph/onehalf', { 'rtp': 'vim' }

" Javascript & React
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'jsx'] }
Plug 'leafgarland/typescript-vim'
Plug 'maxmellon/vim-jsx-pretty', { 'for': ['javascript', 'jsx'] }
Plug 'mattn/emmet-vim'
Plug 'HerringtonDarkholme/yats.vim' " TS Syntax
Plug 'cakebaker/scss-syntax.vim', { 'for': 'sass' }
Plug 'jparise/vim-graphql', { 'for': ['javascript', 'typescript', 'jsx'] }
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'hashivim/vim-terraform'

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
let NERDTreeMinimalUI=28
let NERDTreeDirArrows=1

nnoremap <Space> <Nop>
autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType html,eelixir,javascript,jsx,typescriptreact EmmetInstall
autocmd FileType yaml set inde=
autocmd Filetype tex nnoremap <Leader>mb :Start! xelatex %<CR>
au BufRead,BufNewFile all set wrap linebreak nolist textwidth=0 wrapmargin=0
let base16colorspace=256  " Access colors present in 256 colorspace
lang zh_TW.UTF-8

if (has("termguicolors"))
  set termguicolors
endif

colorscheme onehalfdark
command! -nargs=0 Prettier :CocCommand prettier.formatFile

" coc config
let g:coc_global_extensions = [
  \ 'coc-tsserver',
  \ 'coc-eslint', 
  \ 'coc-prettier', 
  \ 'coc-json',
  \ 'coc-elixir'
  \ ]
set hidden " Some servers have issues with backup files, see #649 set nobackup set nowritebackup " Better display for messages set cmdheight=2 " You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300
set shortmess+=c
set signcolumn=yes

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Remap for rename current word
nmap <F2> <Plug>(coc-rename)

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

function NERDTreeToggleFind()
  if &filetype == 'nerdtree' || exists("g:NERDTree") && g:NERDTree.IsOpen()
    :NERDTreeToggle
  else
    :NERDTreeFind
  endif
endfunction

nnoremap <C-c> "+yy
nnoremap <C-t> :tabe<CR>
nnoremap j gj
nnoremap k gk
inoremap <C-s> <esc>:update<cr>
nnoremap <C-s> :update<cr>
nnoremap <C-p> :Files<cr>
map <silent> <Esc><Esc> :noh<CR>
autocmd FileType netrw set nolist
nnoremap <silent> <F9> :call NERDTreeToggleFind()<CR>
map <F10> :wqa<CR>
" Select all occurrences of selected text
vnoremap // y/\V<C-R>"<CR>

map <Leader>wd :q<cr>
map <Leader>Ts :Colors<cr>
map <Leader>fer :source $MYVIMRC<cr>
map <Leader>pi :PlugInstall<cr>
map <Leader>pg :Tags<cr>
map <silent> <Leader>cl :echom "Use gcc instead!"<CR>

map <Leader>gs :Gstatus<cr>
map <Leader>gp :Gpush<cr>
map <Leader>wm :only<cr>
map <Leader>ga :Git add .<cr>
map <Leader>gc :Gcommit<cr>
map <Leader>ds :ToggleWorkspace<cr>

let g:jsx_ext_required = 0
let g:airline_powerline_fonts = 1
let g:mix_format_on_save = 1
let g:workspace_session_disable_on_args = 1
let g:workspace_autosave = 0
let g:NERDSpaceDelims = 1
" let g:gutentags_file_list_command='ag -l --ignore spec/ --ignore public/'

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

set mouse=a
