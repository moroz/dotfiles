vim.api.nvim_create_autocmd({ 'Filetype' }, {
  pattern = { 'c', 'make', 'go', 'php', 'rust' },
  command = 'setlocal tabstop=4 shiftwidth=4 noexpandtab'
})
