" gem install htmlbeautifier
" npm i -g prettier

augroup fmt
  autocmd!
  autocmd BufWritePre *.rb,*.js,*.jsx,*.ts,*.tsx,*.svelte,*.scss,*.sass,*.lua,*.erb,*.json,*.ex,*.exs undojoin | Neoformat
augroup END
