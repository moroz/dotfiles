" gem install htmlbeautifier
" npm i -g prettier

augroup fmt
  autocmd!
  autocmd BufWritePre *.rb,*.js,*.jsx,*.ts,*.mjs,*.tsx,*.svelte,*.scss,*.sass,*.lua,*.erb,*.json,*.css,*.html,*.tf undojoin | Neoformat
augroup END
