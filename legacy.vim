autocmd StdinReadPre * let s:std_in=1

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')

let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

let test#strategy = "neovim"
let test#neovim#term_position = "vert botright"

set mouse=a

" Press Tab to scroll _down_ a list of auto-completions
let g:SuperTabDefaultCompletionType = "<c-n>"

" rustfmt on write using autoformat
autocmd FileType rust autocmd BufWrite *.rs :Autoformat
autocmd BufWritePost *.swift :silent exec "!swift-format -i '%'"

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

func! s:my_colors_setup() abort
  hi! CocFloating guifg=#eeeeee guibg=#222222
  hi! CocMenuSel  guifg=#f1c40f guibg=#383838
endfunc

call s:my_colors_setup()

augroup colorscheme_coc_setup | au!
  au ColorScheme * call s:my_colors_setup()
augroup END

