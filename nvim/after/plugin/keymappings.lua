local telescope = require('telescope.builtin')
vim.keymap.set('n', '<Leader>Ts', telescope.colorscheme, { noremap = true })
vim.keymap.set('n', '<Leader>bb', telescope.buffers, { noremap = true })
vim.api.nvim_set_keymap('n', "<C-s>", ":w!<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('i', "<C-s>", "<esc>:w!<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', "<F5>", ":LspRestart<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', "<F9>", ":NvimTreeFindFileToggle<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', "<F10>", ":wqa!<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>wd', ':q<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>wm', ':only<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>bb', ':Buffers<cr>', { silent = true, noremap = true })

vim.api.nvim_set_keymap('n', 'j', 'gj', { noremap = true })
vim.api.nvim_set_keymap('n', 'k', 'gk', { noremap = true })

vim.keymap.set('n', '<Leader>mtv', ':TestFile<CR>', { silent = true, noremap = true })

local function get_git_root()
    local dot_git_path = vim.fn.findfile(".git", ".;")
    return vim.fn.fnamemodify(dot_git_path, ":p:h")
end

local function dlv_debug_git_root()
    local git_root = get_git_root()
    vim.fn['delve#runCommand']("debug", "", git_root)
end

vim.api.nvim_create_autocmd('FileType', {
    callback = function()
        vim.keymap.set('n', '<F8>', ':DlvToggleBreakpoint<CR>', { buffer = true, silent = true, noremap = true })
        local bufname = vim.api.nvim_buf_get_name(0)
        if bufname:match('_test.go') then
            vim.keymap.set('n', '<F12>', ':DlvTest<CR>', { noremap = true, buffer = true })
        else
            vim.keymap.set('n', '<F12>', dlv_debug_git_root, { noremap = true, buffer = true })
        end
    end,
    pattern = { 'go' }
})

local function get_path_relative_to_git_root()
    local git_root = get_git_root()
    local file = vim.fn.expand('%:p')
    local relative = file:gsub(git_root, "")
    vim.fn.setreg("+", relative)
end

vim.api.nvim_create_user_command('CopyFilename', get_path_relative_to_git_root, {})
