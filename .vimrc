execute pathogen#infect()
syntax on
filetype plugin indent on

set tabstop=2
set shiftwidth=2
set expandtab
autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab

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
set nobackup
set nowritebackup
set noswapfile
set backspace=2
set history=500
set ruler
set autowrite
set showcmd
set number
set incsearch
set switchbuf=usetab
" set lines=40 columns=169
set hidden

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
set updatetime=300
ab vr vertical resize

if &term =~ '^screen'
    " tmux will send xterm-style keys when its xterm-keys option is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
endif
command! -nargs=0 -bar Qargs execute 'args' QuickfixFilenames()
function! QuickfixFilenames()
  " Building a hash ensures we get each buffer only once
  let buffer_numbers = {}
  for quickfix_item in getqflist()
    let bufnr = quickfix_item['bufnr']
    " Lines without files will appear as bufnr=0
    if bufnr > 0
      let buffer_numbers[bufnr] = bufname(bufnr)
    endif
  endfor
  return join(map(values(buffer_numbers), 'fnameescape(v:val)'))
endfunction


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
let g:switch_mapping = "gs"
