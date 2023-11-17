vim.opt.nu = true
vim.g.base16colorspace=256
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.number = true
vim.opt.hidden = true
vim.opt.laststatus = 2
vim.opt.showcmd = true
vim.opt.swapfile = false
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.lbr = true
vim.opt.smartindent = true
vim.opt.eol = true
vim.opt.autoread = true
vim.opt.foldenable = false
vim.opt.guicursor=""
vim.opt.updatetime = 300
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.opt.termguicolors = true

vim.api.nvim_create_autocmd({'Filetype'}, {
  pattern = {'c', 'make', 'go', 'php', 'rust'},
  command = 'setlocal tabstop=4 shiftwidth=4 noexpandtab'
})

vim.g.mapleader = ' '
vim.cmd.colorscheme('codedark')
