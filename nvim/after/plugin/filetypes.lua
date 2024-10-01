vim.api.nvim_create_autocmd({ 'Filetype' }, {
  pattern = { 'lua' },
  command = 'lua vim.treesitter.stop()'
})

vim.api.nvim_create_autocmd({ 'Filetype' }, {
  pattern = { 'c', 'make', 'go', 'php', 'rust', 'cs' },
  command = 'setlocal tabstop=4 shiftwidth=4 noexpandtab fileformat=unix'
})

vim.api.nvim_create_autocmd({ 'Filetype' }, {
  pattern = 'c',
  command = 'setlocal tabstop=2 shiftwidth=2 noexpandtab'
})

vim.cmd([[source ~/.config/nvim/after/plugin/neoformat.vim]])
