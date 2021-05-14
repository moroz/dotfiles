if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'lambdalisue/nerdfont.vim'
Plug 'lambdalisue/fern.vim'
Plug 'lambdalisue/fern-renderer-nerdfont.vim'
Plug 'lambdalisue/fern-git-status.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
" Plug 'jreybert/vimagit'
Plug 'tpope/vim-dispatch'
" Plug 'lilydjwg/fcitx.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Plug 'sonph/onehalf', { 'rtp': 'vim' }
Plug 'chriskempson/base16-vim'
Plug 'flazz/vim-colorschemes'
" Plug 'romgrk/doom-one.vim'
" Plug 'herrbischoff/cobalt2.vim'
" Plug 'ethantrithon/elementary.vim'

" Testing
Plug 'vim-test/vim-test', { 'for': ['elixir', 'typescript'] }

" Editing
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-commentary'
Plug 'ctjhoa/spacevim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise', { 'for': ['elixir', 'ruby'] }
Plug 'tpope/vim-abolish'
Plug 'airblade/vim-gitgutter'
Plug 'slim-template/vim-slim', { 'for': 'slim' }
Plug 'jiangmiao/auto-pairs'
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'pearofducks/ansible-vim', { 'do': './UltiSnips/generate.sh', 'for': 'ansible' }
Plug 'lifepillar/pgsql.vim', { 'for': 'sql' }

" Elixir
Plug 'elixir-editors/vim-elixir', { 'for': 'elixir' }
Plug 'mhinz/vim-mix-format', { 'for': 'elixir' }
Plug 'elixir-lsp/elixir-ls', { 'for': 'elixir','do': { -> g:ElixirLS.compile() }  }

" Plug 'SirVer/ultisnips', { 'for': ['javascript', 'jsx', 'typescript', 'typescriptreact'] }
Plug 'honza/vim-snippets', { 'for': ['javascript', 'jsx', 'typescript', 'typescriptreact'] }

Plug 'ludovicchabant/vim-gutentags', { 'for': ['javascript', 'jsx', 'typescript', 'typescriptreact', 'elixir'] }

" Rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'ervandew/supertab', { 'for': 'rust' }
Plug 'Chiel92/vim-autoformat', { 'for': 'rust' }

" Javascript & React
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'jsx', 'typescript', 'typescriptreact'] }
Plug 'maxmellon/vim-jsx-pretty', { 'for': ['javascript', 'jsx', 'typescriptreact'] }
Plug 'mattn/emmet-vim', { 'for': ['javascript', 'jsx', 'typescriptreact'] }
Plug 'HerringtonDarkholme/yats.vim', { 'for': ['typescript', 'typescriptreact'] }
Plug 'cakebaker/scss-syntax.vim', { 'for': 'sass' }
" Plug 'shmargum/vim-sass-colors', { 'for': 'sass' }
Plug 'jparise/vim-graphql', { 'for': ['javascript', 'typescript', 'jsx'] }
Plug 'Shougo/vimproc.vim', {'do' : 'make'}

Plug 'evanleck/vim-svelte', {'branch': 'main', 'for': 'svelte' }

" Terraform
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'rhadley-recurly/vim-terragrunt', { 'for': 'terraform' }
Plug 'juliosueiras/vim-terraform-completion', { 'for': 'terraform' }

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
set nofoldenable
" set guicursor=

let mapleader = " "

nnoremap <Space> <Nop>
autocmd Filetype make setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype c setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype php setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd FileType yaml set inde=
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * :checktime
let g:terraform_fmt_on_save=1

" LaTeX
autocmd Filetype tex nnoremap <Leader>mb <Leader>ll
autocmd BufReadPre *.tex let b:vimtex_main = 'main.tex'
let g:vimtex_view_method = 'skim'

au BufRead,BufNewFile all set wrap linebreak nolist textwidth=0 wrapmargin=0
let base16colorspace=256  " Access colors present in 256 colorspace
lang zh_TW.UTF-8

autocmd StdinReadPre * let s:std_in=1

if (has("termguicolors"))
  set termguicolors
endif

command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')

" coc config
let g:coc_global_extensions = [
      \ 'coc-tsserver',
      \ 'coc-eslint',
      \ 'coc-prettier',
      \ 'coc-json',
      \ 'coc-elixir',
      \ 'coc-emmet',
      \ 'coc-css',
      \ 'coc-diagnostic'
      \ ]
set hidden " Some servers have issues with backup files, see #649 set nobackup set nowritebackup " Better display for messages set cmdheight=2 " You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300
set shortmess+=c
set signcolumn=yes

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Remap for rename current word
nmap <F2> <Plug>(coc-rename)

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

nnoremap <C-c> "+yy
nnoremap j gj
nnoremap k gk
inoremap <C-s> <esc>:update<cr>
nnoremap <C-s> :update<cr>
nnoremap <C-p> :Files<cr>
map <silent> <Esc><Esc> :noh<CR>
autocmd FileType netrw set nolist
inoremap <silent> <F9> <esc>
nnoremap <silent> <F9> :Fern . -drawer -reveal=% -toggle<CR>
map <F10> :wqa<CR>
" Select all occurrences of selected text
vnoremap // y/\V<C-R>"<CR>

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

map <Leader>wd :q<cr>
map <Leader>Ts :Colors<cr>
map <Leader>fer :source $MYVIMRC<cr>
map <Leader>pi :PlugInstall<cr>
map <Leader>pg :Tags<cr>

map <Leader>gs :Git<cr>
map <Leader>gp :Dispatch! git push<cr>
map <Leader>wm :only<cr>
map <Leader>ga :Git add .<cr>
map <Leader>gc :Gcommit<cr>
map <Leader>ds :ToggleWorkspace<cr>
map <Leader>mtv :TestFile<CR>
map <Leader>mtr :TestLast<CR>
map <Leader>mta :TestSuite<CR>
map <Leader>mm :Dispatch! mix ecto.migrate<CR>

let g:jsx_ext_required = 0
let g:airline_powerline_fonts = 0
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:mix_format_on_save = 1
let g:workspace_session_disable_on_args = 1
let g:workspace_autosave = 0

let test#strategy = "neovim"
let test#neovim#term_position = "vert botright"

if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    let g:python_host_prog = '/usr/local/bin/python2'
    let g:python3_host_prog = '/opt/homebrew/bin/python3'
    let s:daytime = system("$HOME/.dotfiles/daytime.Darwin")
  else
    let s:daytime = system("$HOME/.dotfiles/daytime")
  endif

  let g:daytime = s:daytime == "DAYTIME\n"
endif

if g:daytime
  colorscheme tender
  " colorscheme codedark
  " let g:airline_theme = 'jellybeans'
else
  colorscheme distinguished
endif

set mouse=a

" Press Tab to scroll _down_ a list of auto-completions
let g:SuperTabDefaultCompletionType = "<c-n>"

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-n>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" rustfmt on write using autoformat
autocmd FileType rust autocmd BufWrite * :Autoformat

" Remap for do codeAction of selected region
function! s:cocActionsOpenFromSelected(type) abort
  execute 'CocCommand actions.open ' . a:type
endfunction
xmap <silent> <leader>a :<C-u>execute 'CocCommand actions.open ' . visualmode()<CR>
nmap <silent> <leader>a :CocAction<CR>

" Use sd to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

let g:fern#renderer = "nerdfont"

let g:AutoPairs = {'(':')', '[':']', '{':'}', "`":"`", '```':'```', '"""':'"""'}

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

function! s:init_fern() abort
  " Use 'select' instead of 'edit' for default 'open' action
  nmap <buffer> d <Plug>(fern-action-remove)
endfunction

augroup fern-custom
  autocmd! *
  autocmd FileType fern call s:init_fern()
augroup END

" https://bernheisel.com/blog/vim-elixir-ls-plug
let g:ElixirLS = {}
let ElixirLS.path = stdpath('config').'/plugged/elixir-ls'
let ElixirLS.lsp = ElixirLS.path.'/release/language_server.sh'
let ElixirLS.cmd = join([
        \ 'cp .release-tool-versions .tool-versions &&',
        \ 'asdf install &&',
        \ 'mix do',
        \ 'local.hex --force --if-missing,',
        \ 'local.rebar --force,',
        \ 'deps.get,',
        \ 'compile,',
        \ 'elixir_ls.release &&',
        \ 'rm .tool-versions'
        \ ], ' ')

function ElixirLS.on_stdout(_job_id, data, _event)
  let self.output[-1] .= a:data[0]
  call extend(self.output, a:data[1:])
endfunction

let ElixirLS.on_stderr = function(ElixirLS.on_stdout)

function ElixirLS.on_exit(_job_id, exitcode, _event)
  if a:exitcode[0] == 0
    echom '>>> ElixirLS compiled'
  else
    echoerr join(self.output, ' ')
    echoerr '>>> ElixirLS compilation failed'
  endif
endfunction

function ElixirLS.compile()
  let me = copy(g:ElixirLS)
  let me.output = ['']
  echom '>>> compiling ElixirLS'
  let me.id = jobstart('cd ' . me.path . ' && git pull && ' . me.cmd, me)
endfunction

function ElixirLS.compile_sync()
  echom '>>> compiling ElixirLS'
  silent call system(g:ElixirLS.cmd)
  echom '>>> ElixirLS compiled'
endfunction

" Then, update the Elixir language server
call coc#config('elixir', {
  \ 'command': g:ElixirLS.lsp,
  \ 'filetypes': ['elixir', 'eelixir']
  \})
call coc#config('elixir.pathToElixirLS', g:ElixirLS.lsp)
