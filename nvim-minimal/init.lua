vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.opt.compatible = false
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.hidden = true
vim.opt.swapfile = false
vim.opt.number = true
vim.opt.guicursor = ""
vim.opt.termguicolors = true

vim.cmd.colorscheme("default")
vim.cmd.syntax('on')

vim.api.nvim_set_keymap('n', "<C-s>", ":w!<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('i', "<C-s>", "<esc>:w!<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>wd', ':q<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>wm', ':only<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>wv', ':vsp<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>wl', '<C-w>l', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>wh', '<C-w>h', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Leader>w-', ':sp<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<F10>', ':wqa<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<F9>', ':Vex<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-p>', ':Files<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Esc><Esc>', ':noh<cr>', { silent = true, noremap = true })

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out,                            "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Setup lazy.nvim
require("lazy").setup({
    spec = {
        'junegunn/fzf',
        'junegunn/fzf.vim',
        'sbdchd/neoformat',
        "lukas-reineke/lsp-format.nvim",

        {
            -- LSP Configuration & Plugins
            'neovim/nvim-lspconfig',
            dependencies = {
                -- Automatically install LSPs to stdpath for neovim
                'williamboman/mason.nvim',
                'williamboman/mason-lspconfig.nvim',

                -- Additional lua configuration, makes nvim stuff amazing!
                'folke/neodev.nvim',
            },
        },

    },
    -- Configure any other settings here. See the documentation for more details.
    -- colorscheme that will be used when installing plugins.
    install = { colorscheme = { "habamax" } },
    -- automatically check for plugin updates
    checker = { enabled = true },
})

vim.api.nvim_create_augroup("fmt", { clear = true })

vim.api.nvim_create_autocmd("BufWritePre", {
    group = "fmt",
    pattern = "*",
    command = "silent! undojoin | Neoformat",
})

require('mason').setup()
require('mason-lspconfig').setup()
require('neodev').setup()
local mason_lspconfig = require('mason-lspconfig')

mason_lspconfig.setup {
    ensure_installed = { "gopls", "clangd", "tailwindcss", "rust_analyzer", "lua_ls" },
    automatic_installation = true,
}

vim.lsp.set_log_level("ERROR")

local on_attach = function(server_name)
    return function(client, bufnr)
        local nmap = function(keys, func, desc)
            if desc then
                desc = 'LSP: ' .. desc
            end

            vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
        end
        nmap('K', vim.lsp.buf.hover, 'Hover Documentation')

        local enable_format_lsps = { 'svelte', 'lua_ls', 'efm', 'elixirls', 'elixir-ls' }
        local enable_format = false
        for i = 1, #enable_format_lsps, 1 do
            if enable_format_lsps[i] == server_name then
                enable_format = true
            end
        end

        if not enable_format then
            return
        end

        require("lsp-format").on_attach(client, bufnr)
    end
end

local servers = {
    gopls = {},
    templ = {},
    elixirls = {},
    svelte = {},
    rubocop = {},
    tailwindcss = {
        filetypes = {
            "astro", "astro-markdown", "eelixir", "erb", "eruby", "gohtml", "haml", "html", "html-eex", "liquid",
            "markdown",
            "mdx", "css", "less", "postcss", "sass", "scss", "javascript", "javascriptreact", "typescript",
            "typescriptreact",
            "svelte", "templ", "gohtmltmpl"
        }
    },

    lua_ls = {
        Lua = {
            workspace = { checkThirdParty = false },
            telemetry = { enable = false },
        },
    },
}

mason_lspconfig.setup_handlers {
    function(server_name)
        require('lspconfig')[server_name].setup {
            on_attach = on_attach(server_name),
            settings = servers[server_name],
            filetypes = (servers[server_name] or {}).filetypes,
            init_options = (servers[server_name] or {}).init_options,
        }
    end,
}
