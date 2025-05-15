vim.api.nvim_create_augroup("fmt", {})

vim.api.nvim_create_autocmd(
  "BufWritePost",
  {
    pattern = "*.templ",
    group = "fmt",
    callback = function()
      vim.cmd("silent !templ fmt %")
      vim.cmd("edit")
    end,
  }
)

vim.api.nvim_create_autocmd("BufWritePre", {
  group = "fmt",
  pattern = { '*.rb', '*.js', '*.jsx', '*.ts', '*.mjs', '*.tsx', '*.svelte', '*.scss', '*.sass', '*.lua', '*.erb', '*.json', '*.css', '*.html', '*.tf', '*.c', '*.h', '*.astro' },
  command = "silent! undojoin | Neoformat",
})


vim.api.nvim_create_autocmd("BufNewFile", {
  pattern = "*.rb",
  callback = function()
    vim.api.nvim_buf_set_lines(0, 0, 0, false, { "# frozen_string_literal: true", "" })
  end,
})

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
