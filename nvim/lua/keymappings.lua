vim.keymap.set('n', '<C-p>', require('telescope.builtin').git_files, { noremap = true })
vim.api.nvim_set_keymap('n', "<C-s>", ":w<cr>", { silent=true, noremap=true })
vim.api.nvim_set_keymap('i', "<C-s>", "<esc>:w<cr>", { silent=true, noremap=true })
vim.api.nvim_set_keymap('n', "<F9>", ":NvimTreeFindFileToggle<cr>", { silent=true, noremap=true })
vim.api.nvim_set_keymap('n', "<F10>", ":wqa!<cr>", { silent=true, noremap=true })
vim.api.nvim_set_keymap('', '<Leader>wd', ':q<cr>', { silent = true, noremap = true })

vim.api.nvim_set_keymap('n', 'j', 'gj', { noremap = true })
vim.api.nvim_set_keymap('n', 'k', 'gk', { noremap = true })

