vim.api.nvim_create_autocmd({ 'Filetype' }, {
  pattern = { 'c', 'make', 'go', 'php', 'rust' },
  command = 'setlocal tabstop=4 shiftwidth=4 noexpandtab'
})

vim.api.nvim_create_autocmd({ 'Filetype' }, {
  pattern = 'c',
  command = 'setlocal tabstop=2 shiftwidth=2 noexpandtab'
})
