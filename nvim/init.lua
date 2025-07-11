-- vim: ts=2 sts=2 sw=2 et
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.g.user_emmet_install_global = 0

local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- [[ Configure plugins ]]
-- NOTE: Here is where you install your plugins.
--  You can configure plugins using the `config` key.
--
--  You can also configure plugins after the setup call,
--    as they will be available in your neovim runtime.
require('lazy').setup({
  'tpope/vim-surround',
  'tpope/vim-abolish',

  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = {
      'RRethy/nvim-treesitter-endwise',
    },
    config = function()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
        ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "javascript", "html", "go", "rust", "elixir", "ruby", "heex" },
        sync_install = false,
        highlight = { enable = true },
        indent = { enable = true },
      })
    end
  },


  "flazz/vim-colorschemes",
  { "miikanissi/modus-themes.nvim", priority = 1000 },
  { "protesilaos/tempus-themes-vim" },

  -- NOTE: This is where your plugins related to LSP can be installed.
  --  The configuration is done below. Search for lspconfig to find it below.
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
      -- Automatically install LSPs to stdpath for neovim
      -- 'williamboman/mason.nvim',
      -- 'williamboman/mason-lspconfig.nvim',

      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim', opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      'folke/neodev.nvim',
    },
  },

  {
    -- Autocompletion
    'hrsh7th/nvim-cmp',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',

      -- Adds LSP completion capabilities
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
    },
  },

  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    opts = {} -- this is equalent to setup({}) function
  },

  {
    -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = false,
        component_separators = '|',
        section_separators = '',
      },
    },
  },

  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      -- Fuzzy Finder Algorithm which requires local dependencies to be built.
      -- Only load if `make` is available. Make sure you have the system
      -- requirements installed.
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        -- NOTE: If you are having trouble with this installation,
        --       refer to the README for telescope-fzf-native for more instructions.
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
    },
  },

  -- {
  --   "CopilotC-Nvim/CopilotChat.nvim",
  --   dependencies = {
  --     { "github/copilot.vim" },                       -- or zbirenbaum/copilot.lua
  --     { "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
  --   },
  --   build = "make tiktoken",                          -- Only on MacOS or Linux
  --   opts = {
  --     -- See Configuration section for options
  --   },
  --   -- See Commands section for default commands if you want to lazy load on them
  -- },

  'ctjhoa/spacevim',
  { 'h-hg/fcitx.nvim' },
  { 'elixir-editors/vim-elixir', ft = 'elixir' },
  { 'hashivim/vim-terraform',    ft = 'terraform' },
  "lukas-reineke/lsp-format.nvim",
  { 'fatih/vim-go',      ft = { 'go', 'gohtmltmpl' } },
  {
    'sebdah/vim-delve',
    ft = 'go',
    init = function()
      vim.g.delve_new_command = "tabnew"
    end
  },
  { 'joerdav/templ.vim', ft = 'templ' },
  'sbdchd/neoformat',
  'vim-test/vim-test',
  'mattn/emmet-vim',
  { 'leafOfTree/vim-svelte-plugin', ft = 'svelte' },
  -- "ludovicchabant/vim-gutentags",
  {
    "nvim-tree/nvim-tree.lua",
    cmd = {
      'NvimTreeToggle',
      'NvimTreeOpen',
    },
    config = function()
      require('nvim-tree').setup({
        renderer = {
          icons = {
            glyphs = {
              default = "",
              symlink = "",
              folder = {
                default = "",
                empty = "",
                empty_open = "",
                open = "",
                symlink = "",
                symlink_open = "",
                arrow_open = "-",
                arrow_closed = "+"
              },
              git = {
                unstaged = "[U]",
                staged = "✓",
                unmerged = " ",
                renamed = "R",
                untracked = "[?]",
                deleted = "[D]",
                ignored = "◌",
              },
            },
          }
        }
      })
    end
  },
}, {})


-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})
