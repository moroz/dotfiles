call plug#begin('~/.config/nvim/plugged')
" Plugins will go here in the middle.
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/nerdcommenter'
Plug 'vim-syntastic/syntastic'
Plug 'jnurmine/Zenburn'
Plug 'rking/ag.vim'
Plug 'tpope/vim-endwise'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'vim-scripts/fcitx.vim'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'flazz/vim-colorschemes'
Plug 'tpope/vim-rails'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
Plug 'Valloric/YouCompleteMe'
Plug 'ternjs/tern_for_vim'
Plug 'easymotion/vim-easymotion'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'tpope/vim-dispatch'
Plug 'radenling/vim-dispatch-neovim'
call plug#end()

set tabstop=2
set shiftwidth=2
set expandtab
set number
set path+=**
set hidden
set laststatus=2
set showcmd
set noswapfile
set incsearch
set ignorecase
set lbr

autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType yaml set inde=
autocmd Filetype tex command Tex Dispatch! xelatex %
au BufRead,BufNewFile all set wrap linebreak nolist textwidth=0 wrapmargin=0
map <Leader> <Plug>(easymotion-prefix)
" Fix indentation on entire file
map <Leader>ri mzgg=G`z

" Leader shortcuts for Rails commands
map <Space>m :Emodel 
map <Space>c :Econtroller 
map <Space>v :Eview 
map <Space>s :Eschema<cr>
map <Space>e :e Gemfile<cr>
map <Space>i :Emigration 

let NERDTreeMinimalUI=25
let NERDTreeDirArrows=1

nmap <Leader>ev :tabedit $MYVIMRC<CR>
nnoremap <C-Up> :m .-2<CR>==
nnoremap <C-Down> :m +1<CR>==
vmap <C-Down> :m'>+<cr>`<my`>mzgv`yo`z
vmap <C-Up> :m'<-2<cr>`>my`<mzgv`yo`z
nnoremap <C-Left> <c-w>h
nnoremap <C-Right> <c-w>l
nnoremap <C-j> <c-w>j
nnoremap <C-h> <c-w>h
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l
nnoremap j gj
nnoremap k gk
nnoremap <F5> :Buffers<CR>
map <F9> :NERDTreeToggle<CR>
nnoremap <F6> :%y +<CR>
nnoremap <C-p> :Files<CR>
nnoremap <F3> :Tags<CR>
inoremap <C-s> <esc>:update<cr>
inoremap <C-j> <esc>:wq<cr>
nnoremap <C-s> :update<cr>
nnoremap <C-j> :wq<cr>
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
"nnoremap <space>ge :Gedit<CR>
"nnoremap <space>gr :Gread<CR>
"nnoremap <space>gw :Gwrite<CR><CR>
"nnoremap <space>gl :silent! Glog<CR>:bot copen<CR>
"nnoremap <space>gp :Ggrep<Space>
"nnoremap <space>gm :Gmove<Space>
"nnoremap <space>gb :Git branch<Space>
"nnoremap <space>go :Git checkout<Space>
nnoremap <space>gp :Dispatch! git push origin master<CR>
"nnoremap <space>gpl :Dispatch! git pull<CR>

if executable('ag')
    " Note we extract the column as well as the file and line number
    set grepprg=ag\ --nogroup\ --nocolor\ --column
    set grepformat=%f:%l:%c%m
endif

set wildmode=list:longest,list:full
set complete=.,w,t
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_eruby_ruby_quiet_messages =
    \ {'regex': 'possibly useless use of a variable in void context'}
set tags=./tags
let g:easytags_dynamic_files = 1
let g:easytags_async = 0
let g:easytags_events = ['BufWritePost']
let g:jsx_ext_required = 0
colorscheme jellybeans

function! NumberToggle()
  if(&relativenumber == 1)
    set nornu
  else
    set relativenumber
  endif
endfunc

nnoremap <C-n> :call NumberToggle()<cr>
