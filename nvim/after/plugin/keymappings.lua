local telescope = require('telescope.builtin')
vim.keymap.set('n', '<Leader>Ts', telescope.colorscheme, { noremap = true })
vim.keymap.set('n', '<Leader>bb', telescope.buffers, { noremap = true })
vim.keymap.set('n', '<Leader>pg', telescope.tags, { noremap = true })
vim.api.nvim_set_keymap('n', "<C-s>", ":w<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('i', "<C-s>", "<esc>:w<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', "<F5>", ":LspRestart<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', "<F9>", ":NvimTreeFindFileToggle<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', "<F10>", ":wqa!<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>wd', ':q<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>wm', ':only<cr>', { silent = true, noremap = true })

vim.api.nvim_set_keymap('n', 'j', 'gj', { noremap = true })
vim.api.nvim_set_keymap('n', 'k', 'gk', { noremap = true })

local neogit = require("neogit")
vim.keymap.set('n', '<Leader>gg', neogit.open)

vim.keymap.set('n', '<Leader>mtv', ':TestFile<CR>', { silent = true, noremap = true })

local function get_git_root()
  local dot_git_path = vim.fn.finddir(".git", ".;")
  return vim.fn.fnamemodify(dot_git_path, ":p:h:h")
end

local function dlv_debug_git_root()
  local git_root = get_git_root()
  vim.fn['delve#runCommand']("debug", "", git_root)
end

vim.cmd [[
  autocmd FileType go nnoremap <buffer> <silent> <F8> :DlvToggleBreakpoint<CR>
]]

vim.api.nvim_create_autocmd('FileType', {
  callback = function()
    vim.keymap.set('n', '<F12>', dlv_debug_git_root, { noremap = true, buffer = true })
  end,
  pattern = { 'go' }
})
