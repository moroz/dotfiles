if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    let s:linux = 0
    let g:python_host_prog = '$HOME/.asdf/shims/python3'
    let g:python3_host_prog = "$HOME/.asdf/shims/python3"
    let s:daytime = system("$HOME/.dotfiles/darkmode.Darwin")
  else
    let s:linux = 1
    let s:daytime = system("$HOME/.dotfiles/daytime")
    let g:fcitx5_remote = '/usr/bin/fcitx5-remote'
  endif

  let g:daytime = s:daytime == "DAYTIME\n"
endif

call plug#begin('~/.config/nvim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'lambdalisue/nerdfont.vim'
" Plug 'lambdalisue/fern.vim', { 'branch': 'main' }
" Plug 'lambdalisue/fern-renderer-nerdfont.vim'
" Plug 'lambdalisue/fern-git-status.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'TimUntersberger/neogit'
Plug 'nvim-lua/plenary.nvim'
Plug 'tpope/vim-dispatch'

Plug 'nvim-tree/nvim-web-devicons' " optional, for file icons
Plug 'nvim-tree/nvim-tree.lua'

if s:linux
  Plug 'lilydjwg/fcitx.vim'
endif

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Color schemes
Plug 'sonph/onehalf', { 'rtp': 'vim' }
Plug 'chriskempson/base16-vim'
Plug 'flazz/vim-colorschemes'
" Plug 'romgrk/doom-one.vim'
" Plug 'ethantrithon/elementary.vim'
" Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
" Plug 'catppuccin/nvim'
Plug 'sainnhe/everforest'

" Testing
Plug 'vim-test/vim-test', { 'for': ['elixir', 'typescript', 'ruby', 'javascript', 'javascriptreact', 'rust', 'go'] }

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
" Plug 'pearofducks/ansible-vim', { 'do': './UltiSnips/generate.sh', 'for': 'ansible' }
Plug 'lifepillar/pgsql.vim', { 'for': 'sql' }

" Elixir
Plug 'elixir-editors/vim-elixir'
Plug 'mhinz/vim-mix-format', { 'for': 'elixir' }

Plug 'SirVer/ultisnips', { 'for': ['javascript', 'jsx', 'typescript', 'typescriptreact', 'markdown', 'swift'] }
Plug 'mlaursen/vim-react-snippets'

Plug 'ludovicchabant/vim-gutentags', { 'for': ['javascript', 'jsx', 'typescript', 'typescriptreact', 'elixir', 'rust', 'go'] }

" Ruby
Plug 'tpope/vim-rails', { 'for': 'ruby' }

" Rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'ervandew/supertab', { 'for': 'rust' }
Plug 'Chiel92/vim-autoformat', { 'for': [ 'rust' , 'go'] }

" Go
Plug 'fatih/vim-go' ", { 'for': 'go' }

" Javascript & React
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'jsx', 'typescript', 'typescriptreact'] }
Plug 'maxmellon/vim-jsx-pretty', { 'for': ['javascript', 'jsx', 'typescriptreact'] }
Plug 'mattn/emmet-vim', { 'for': ['javascript', 'jsx', 'typescriptreact', 'html', 'eelixir'] }
Plug 'HerringtonDarkholme/yats.vim', { 'for': ['typescript', 'typescriptreact'] }
Plug 'cakebaker/scss-syntax.vim', { 'for': 'sass' }
" Plug 'shmargum/vim-sass-colors', { 'for': 'sass' }
Plug 'jparise/vim-graphql', { 'for': ['javascript', 'typescript', 'jsx'] }
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'digitaltoad/vim-pug', { 'for': 'pug' }
Plug 'pantharshit00/vim-prisma', { 'for': 'prisma' }

Plug 'evanleck/vim-svelte', {'branch': 'main', 'for': 'svelte' }
Plug 'leafOfTree/vim-vue-plugin'

" Terraform
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'rhadley-recurly/vim-terragrunt', { 'for': 'terraform' }
Plug 'juliosueiras/vim-terraform-completion', { 'for': 'terraform' }

call plug#end()

let mapleader = " "

nnoremap <Space> <Nop>
autocmd Filetype c,make,go,php,rust setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype php setlocal tabstop=4 shiftwidth=4 noexpandtab
autocmd Filetype swift setlocal tabstop=2 shiftwidth=2 expandtab
autocmd FileType yaml set inde=
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * :checktime
let g:terraform_fmt_on_save=1
autocmd BufRead,BufNewFile *.slimleex set filetype=slim

"" LaTeX
autocmd Filetype tex nnoremap <Leader>mb <Leader>ll
autocmd BufReadPre *.tex let b:vimtex_main = 'main.tex'

au BufRead,BufNewFile all set wrap linebreak nolist textwidth=0 wrapmargin=0
let base16colorspace=256  "" Access colors present in 256 colorspace

autocmd StdinReadPre * let s:std_in=1

command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')

" coc config
set updatetime=300
set shortmess+=c
set signcolumn=yes

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

autocmd FileType netrw set nolist
inoremap <silent> <F9> <esc>

" Select all occurrences of selected text
vnoremap // y/\V<C-R>"<CR>

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

let test#strategy = "neovim"
let test#neovim#term_position = "vert botright"

if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    let s:linux = 0
    let g:python_host_prog = '/usr/local/bin/python2'
    let g:python3_host_prog = '/opt/homebrew/bin/python3'
    let s:daytime = system("$HOME/.dotfiles/darkmode.Darwin")
  else
    let s:linux = 1
    let s:daytime = system("$HOME/.dotfiles/daytime")
  endif

  let g:daytime = s:daytime == "DAYTIME\n" || s:daytime == "DAYTIME"

  if g:daytime
    colorscheme cobalt2
  else
    colorscheme distinguished
  endif

  if $VIM_COLORSCHEME != ""
    colorscheme $VIM_COLORSCHEME
  endif
endif

set mouse=a

" Press Tab to scroll _down_ a list of auto-completions
let g:SuperTabDefaultCompletionType = "<c-n>"

" rustfmt on write using autoformat
autocmd FileType rust autocmd BufWrite *.rs :Autoformat
autocmd BufWritePost *.swift :silent exec "!swift-format -i '%'"

xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)w

"" Use sd to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>
"" Make <CR> to accept selected completion item or notify coc.nvim to format
"" <C-g>u breaks current undo, please make your own choice.
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
      \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

nnoremap <Leader>mr :vert ter swift %<CR>a

func! s:my_colors_setup() abort
  hi! CocFloating guifg=#eeeeee guibg=#222222
  hi! CocMenuSel  guifg=#f1c40f guibg=#383838
endfunc

call s:my_colors_setup()

augroup colorscheme_coc_setup | au!
  au ColorScheme * call s:my_colors_setup()
augroup END

