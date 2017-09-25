call plug#begin('~/.config/nvim/plugged')
" Plugins will go here in the middle.
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'vim-syntastic/syntastic'
Plug 'tpope/vim-endwise'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
"Plug 'vim-scripts/fcitx.vim'
Plug 'mmai/vim-zenmode'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'flazz/vim-colorschemes'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
Plug 'Valloric/YouCompleteMe'
Plug 'ternjs/tern_for_vim'
Plug 'easymotion/vim-easymotion'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'vim-ruby/vim-ruby'
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'cakebaker/scss-syntax.vim', { 'for': 'sass' }
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'
Plug 'alvan/vim-closetag'
Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
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

" Leader shortcuts for Rails commands
"map <Space>m :Emodel 
"map <Space>c :Econtroller 
"map <Space>i :Emigration 

let NERDTreeMinimalUI=25
let NERDTreeDirArrows=1

nmap <Leader>ev :tabedit $MYVIMRC<CR>
nnoremap <M-Up> :m .-2<CR>==
nnoremap <M-Down> :m +1<CR>==
vmap <M-Down> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-Up> :m'<-2<cr>`>my`<mzgv`yo`z
nnoremap <C-j> <c-w>j
nnoremap <C-h> <c-w>h
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
nnoremap j gj
nnoremap k gk
nnoremap <F5> :Buffers<CR>
nnoremap <F4> :GundoToggle<CR>
map <F9> :NERDTreeToggle<CR>
nnoremap <F6> :%y +<CR>
nnoremap <C-p> :Files<CR>
nnoremap <F3> :Tags<CR>
inoremap <D-s> <esc>:w<cr>
nnoremap <D-s> :w<cr>
inoremap <C-s> <esc>:update<cr>
nnoremap <C-s> :update<cr>
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>
map <F10> :bufdo update<CR>:bufdo q<CR>
map <silent> <Esc><Esc> :noh<CR>
command! Uall bufdo update
command! Bdall %bd|e#

"https://www.reddit.com/r/vim/comments/21f4gm/best_workflow_when_using_fugitive/
" fugitive git bindings
nnoremap <space>ga :Git add %:p<CR><CR>
nnoremap <space>gs :Gstatus<CR>
nnoremap <space>gl :Git add .<CR><CR>
nnoremap <space>gc :Gcommit -v -q<CR>
nnoremap <space>gt :Gcommit -v -q %:p<CR>
nnoremap <space>gd :Gdiff<CR>
nnoremap <space>gp :Dispatch! git push origin master<CR>

if executable('ag')
    " Note we extract the column as well as the file and line number
    set grepprg=ag\ --nogroup\ --nocolor\ --column
    set grepformat=%f:%l:%c%m
endif
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0
"let g:syntastic_scss_sass_quiet_messages = {
    "\ "regex": 'File to import not found or unreadable', }
"let g:syntastic_eruby_ruby_quiet_messages =
    "\ {'regex': 'possibly useless use of a variable in void context'}
set tags=./tags
let g:easytags_dynamic_files = 1
let g:easytags_async = 0
let g:easytags_events = ['BufWritePost']
let g:jsx_ext_required = 0
let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.haml,*.erb"
let g:airline_powerline_fonts = 1
"Zenmode
let g:zenmode_background = "dark"
let g:zenmode_colorscheme = "solarized"
colorscheme gruvbox
set background=dark
let g:python_host_prog = '/usr/local/bin/python2'
let g:python3_host_prog = '/usr/local/bin/python3'

function! NumberToggle()
  if(&relativenumber == 1)
    set nornu
  else
    set relativenumber
  endif
endfunc

nnoremap <C-n> :call NumberToggle()<cr>
set mouse=a
set guicursor=
