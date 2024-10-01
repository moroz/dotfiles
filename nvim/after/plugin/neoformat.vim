augroup fmt
  autocmd!
  autocmd BufWritePre *.rb,*.js,*.jsx,*.ts,*.tsx,*.svelte,*.scss,*.sass,*.lua undojoin | Neoformat
augroup END
