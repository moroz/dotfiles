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
  pattern = { '*.rb', '*.js', '*.jsx', '*.ts', '*.mjs', '*.tsx', '*.svelte', '*.scss', '*.sass', '*.lua', '*.erb', '*.json', '*.css', '*.html', '*.tf', '*.c', '*.h', '*.astro', '*.ex', '*.exs' },
  command = "silent! undojoin | Neoformat",
})

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "*.heex",
  callback = function()
    local view = vim.fn.winsaveview()
    vim.cmd("silent! !mix format %")
    vim.cmd("edit!")
    vim.fn.winrestview(view)
  end,
})

vim.api.nvim_create_autocmd("BufReadPost", {
  pattern = "*.rb",
  callback = function()
    if vim.fn.line('$') == 1 and vim.fn.getline(1) == '' then
      vim.api.nvim_buf_set_lines(0, 0, 0, false, { "# frozen_string_literal: true", "" })
    end
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

vim.api.nvim_create_autocmd("FileType", {
  pattern = {
    "astro", "astro-markdown", "eelixir", "erb", "eruby", "gohtml", "haml", "html", "heex", "markdown",
    "mdx", "css", "sass", "scss", "javascript", "javascriptreact", "typescript", "typescriptreact",
    "svelte", "templ", "gohtmltmpl"
  },
  callback = function(_)
    vim.cmd("EmmetInstall")
    require("cmp").setup.buffer({ enabled = false })

    vim.cmd([[
      imap <expr> <buffer> <tab> emmet#expandAbbrIntelligent("\<tab>")
    ]])
  end,
})
