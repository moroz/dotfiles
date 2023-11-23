local telescope = require('telescope.builtin')
vim.keymap.set('n', '<Leader>Ts', telescope.colorscheme, { noremap = true })
vim.keymap.set('n', '<Leader>bb', telescope.buffers, { noremap = true })
vim.keymap.set('n', '<Leader>pg', telescope.tags, { noremap = true })
vim.api.nvim_set_keymap('n', "<C-s>", ":w<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('i', "<C-s>", "<esc>:w<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', "<F9>", ":NvimTreeFindFileToggle<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', "<F10>", ":wqa!<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>wd', ':q<cr>', { silent = true, noremap = true })

vim.api.nvim_set_keymap('n', 'j', 'gj', { noremap = true })
vim.api.nvim_set_keymap('n', 'k', 'gk', { noremap = true })

local neogit = require("neogit")
vim.keymap.set('n', '<Leader>gg', neogit.open)

vim.keymap.set('n', '<Leader>mtv', ':TestFile<CR>', { silent = true, noremap = true })
