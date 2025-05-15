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
vim.wo.signcolumn = 'yes'
vim.opt.smartcase = true
vim.opt.ignorecase = true
vim.opt.smartindent = true
vim.o.fileformat = 'unix'

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
vim.api.nvim_set_keymap('n', '<F9>', ':Ex<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-p>', ':Files<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Esc><Esc>', ':noh<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Leader><Tab>', '<C-^>', { silent = true, noremap = true })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<Leader>mtv', ':TestFile<CR>', { silent = true, noremap = true })
vim.keymap.set('n', '<Leader>bb', ':Buffers<CR>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', 'j', 'gj', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', 'k', 'gk', { silent = true, noremap = true })

vim.g.fzf_layout = { window = { width = 0.9, height = 0.8, relative = true } }

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

-- Setup lazy.nvim
require("lazy").setup({
	spec = {
		'tpope/vim-surround',
		'tpope/vim-abolish',
		'junegunn/fzf',
		'junegunn/fzf.vim',
		'sbdchd/neoformat',
		"lukas-reineke/lsp-format.nvim",
		'vim-test/vim-test',
		'h-hg/fcitx.nvim',
		{
			'mattn/emmet-vim',
			init = function()
				vim.cmd([[
        imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
      ]])
			end
		},
		{ 'fatih/vim-go', ft = { 'go', 'gohtmltmpl' } },
		{
			'sebdah/vim-delve',
			ft = 'go',
			init = function()
				vim.g.delve_new_command = "tabnew"
			end
		},
		{
			'windwp/nvim-autopairs',
			event = "InsertEnter",
			opts = {} -- this is equalent to setup({}) function
		},
		{
			"forest-nvim/sequoia.nvim",
			lazy = false,
		},
		{
			"rebelot/kanagawa.nvim",
			lazy = false,
		},
		{
			'mason-org/mason.nvim',
			version = 'v1.11.0'
		},
		{
			'mason-org/mason-lspconfig.nvim',
			version = 'v1.32.0'
		},
		{
			-- LSP Configuration & Plugins
			'neovim/nvim-lspconfig',
			dependencies = {
				-- -- Automatically install LSPs to stdpath for neovim
				-- 'williamboman/mason.nvim',
				-- 'williamboman/mason-lspconfig.nvim',
			},
		},
		{
			-- Autocompletion
			'hrsh7th/nvim-cmp',
			dependencies = {
				-- Adds LSP completion capabilities
				'hrsh7th/cmp-nvim-lsp',
				'hrsh7th/cmp-path',
			},
		},
		{
			"nvim-treesitter/nvim-treesitter",
			build = ":TSUpdate",
			config = function()
				local configs = require("nvim-treesitter.configs")

				configs.setup({
					ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "javascript", "html", "go", "rust", "elixir", "ruby" },
					sync_install = false,
					highlight = { enable = true },
					indent = { enable = true },
				})
			end
		}
	},
	-- automatically check for plugin updates
	checker = { enabled = true },
})

vim.cmd.colorscheme("kanagawa")
vim.api.nvim_create_augroup("fmt", { clear = true })

vim.api.nvim_create_autocmd("BufWritePre", {
	group = "fmt",
	pattern = "*",
	command = "silent! undojoin | Neoformat",
})

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
	clangd = {
		filetypes = { "c", "cpp" },
	},
	gopls = {},
	templ = {},
	elixirls = {},
	svelte = {},
	rubocop = {},
	tailwindcss = {
		filetypes = {
			"astro", "astro-markdown", "eelixir", "erb", "eruby", "gohtml", "haml", "html", "html-eex", "liquid", "markdown",
			"mdx", "css", "less", "postcss", "sass", "scss", "javascript", "javascriptreact", "typescript", "typescriptreact",
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


local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
capabilities['offsetEncoding'] = { 'utf-16' }

require('mason').setup()
require('mason-lspconfig').setup()
local mason_lspconfig = require('mason-lspconfig')

mason_lspconfig.setup {
	ensure_installed = { "gopls", "clangd", "tailwindcss", "rust_analyzer", "lua_ls", "rubocop", "ts_ls" },
	automatic_installation = true,
}

mason_lspconfig.setup_handlers {
	function(server_name)
		require('lspconfig')[server_name].setup {
			capabilities = capabilities,
			on_attach = on_attach(server_name),
			settings = servers[server_name],
			filetypes = (servers[server_name] or {}).filetypes,
			init_options = (servers[server_name] or {}).init_options,
		}
	end,
}

local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
	callback = function()
		vim.highlight.on_yank()
	end,
	group = highlight_group,
	pattern = '*',
})

vim.api.nvim_create_autocmd({ 'Filetype' }, {
	pattern = { 'make', 'go', 'php', 'rust', 'cs', 'c' },
	command = 'setlocal tabstop=4 shiftwidth=4 noexpandtab fileformat=unix'
})

vim.api.nvim_create_autocmd({ 'Filetype' }, {
	pattern = { 'c', 'lua', 'elixir', 'html' },
	command = 'setlocal tabstop=2 shiftwidth=2 noexpandtab fileformat=unix'
})

vim.api.nvim_create_autocmd("BufNewFile", {
	pattern = "*.rb",
	callback = function()
		vim.api.nvim_buf_set_lines(0, 0, 0, false, { "# frozen_string_literal: true", "" })
	end,
})

local cmp = require 'cmp'
cmp.setup {
	sources = {
		{ name = 'nvim_lsp' },
		{ name = "emmet_vim" },
		{ name = "path" },
	},
	completion = {
		autocomplete = false,
	},
	mapping = cmp.mapping.preset.insert {
		['<C-n>'] = cmp.mapping.select_next_item(),
		['<C-p>'] = cmp.mapping.select_prev_item(),
		['<C-d>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete {},
		['<CR>'] = cmp.mapping.confirm {
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		},
		['<Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			else
				fallback()
			end
		end, { 'i', 's' }),
		['<S-Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			else
				fallback()
			end
		end, { 'i', 's' }),
	},
}
