vim.cmd('source $HOME/.config/nvim/legacy.vim')

require('packer').startup(function (use)
  use 'nvim-tree/nvim-web-devicons'
  use 'nvim-tree/nvim-tree.lua'
  use 'junegunn/fzf.vim'
  use 'TimUntersberger/neogit'
  use 'nvim-lua/plenary.nvim'
  use 'tpope/vim-dispatch'
  use 'vim-airline/vim-airline'
  use 'vim-airline/vim-airline-themes'
  use 'chriskempson/base16-vim'
  use 'flazz/vim-colorschemes'

  -- Editing
  use 'easymotion/vim-easymotion'
  use 'tpope/vim-commentary'
  use 'ctjhoa/spacevim'
  use 'tpope/vim-surround'
  use 'tpope/vim-abolish'
  use 'airblade/vim-gitgutter'
  use 'jiangmiao/auto-pairs'
  use 'tmux-plugins/vim-tmux-focus-events'
  use 'elixir-editors/vim-elixir'
  use 'mlaursen/vim-react-snippets'

  use 'fatih/vim-go'

  use {
    'mhinz/vim-mix-format', ft = { 'elixir' }
  }

  -- Javascript & React
  use {
    'pangloss/vim-javascript',
    ft = { 'javascript', 'jsx', 'typescript', 'typescriptreact' }
  }
  use {
    'maxmellon/vim-jsx-pretty',
    ft = { 'javascript', 'jsx', 'typescriptreact' }
  }
  use {
   'mattn/emmet-vim',
   ft = { 'javascript', 'jsx', 'typescriptreact', 'html', 'eelixir' }
  }
  use {
    'HerringtonDarkholme/yats.vim',
    ft = { 'typescript', 'typescriptreact' }
  }
  use {
    'cakebaker/scss-syntax.vim',
    ft = { 'sass' }
  }
  use {
    'jparise/vim-graphql',
    ft = { 'javascript', 'typescript', 'jsx' }
  }
  use { 'Shougo/vimproc.vim', run = 'make' }

  use {'neoclide/coc.nvim', branch = 'release'}
  use 'lambdalisue/nerdfont.vim'
  use {'junegunn/fzf', dir = '~/.fzf', run = './install --all'}

  -- if s:linux
  --   use 'lilydjwg/fcitx.vim'
  -- end

  -- Color schemes
  use {'sonph/onehalf', rtp = 'vim'}

  -- Testing
  use {'vim-test/vim-test', ft = {'elixir', 'typescript', 'ruby', 'javascript', 'javascriptreact', 'rust', 'go'}}

  -- Editing
  use {'tpope/vim-endwise', ft = {'elixir', 'ruby'}}
  use {'lervag/vimtex', ft = 'tex'}
  use {'lifepillar/pgsql.vim', ft = 'sql'}

  -- Elixir
  use {'SirVer/ultisnips', ft = {'javascript', 'jsx', 'typescript', 'typescriptreact', 'markdown', 'swift'}}
  use {'ludovicchabant/vim-gutentags', ft = {'javascript', 'jsx', 'typescript', 'typescriptreact', 'elixir', 'rust', 'go'}}

  -- Ruby
  use {'tpope/vim-rails', ft = 'ruby'}

  -- Rust
  use {'rust-lang/rust.vim', ft = 'rust'}
  use 'ervandew/supertab'
  use {'Chiel92/vim-autoformat', ft = {'rust', 'go'}}

  -- Terraform
  use {'hashivim/vim-terraform', ft = 'terraform'}
  use {'rhadley-recurly/vim-terragrunt', ft = 'terraform'}
  use {'juliosueiras/vim-terraform-completion', ft = 'terraform'}

end)

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

-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- set termguicolors to enable highlight groups
vim.opt.termguicolors = true

-- empty setup using defaults
require("nvim-tree").setup()

local neogit = require("neogit")

vim.api.nvim_set_keymap('n', '<leader>gp', '<Cmd>Neogit push<CR>', { noremap = true, silent = true })

vim.api.nvim_set_keymap('n', '<C-c>', '"+yy', { noremap = true })
vim.api.nvim_set_keymap('n', 'j', 'gj', { noremap = true })
vim.api.nvim_set_keymap('n', 'k', 'gk', { noremap = true })

vim.api.nvim_set_keymap('', '<Leader>wd', ':q<cr>', {})
vim.api.nvim_set_keymap('', '<Leader>Ts', ':Colors<cr>', {})
vim.api.nvim_set_keymap('', '<Leader>fer', ':source $MYVIMRC<cr>', {})
vim.api.nvim_set_keymap('', '<Leader>pi', ':PlugInstall<cr>', {})
vim.api.nvim_set_keymap('', '<Leader>pg', ':Tags<cr>', {})

vim.api.nvim_set_keymap('', '<Leader>gg', ':Neogit<cr>', {})
vim.api.nvim_set_keymap('', '<Leader>gp', ':NeogitPushPopup<cr>', {})
vim.api.nvim_set_keymap('', '<Leader>wm', ':only<cr>', {})
vim.api.nvim_set_keymap('', '<Leader>mtv', ':TestFile<CR>', {})
vim.api.nvim_set_keymap('', '<Leader>mtr', ':TestLast<CR>', {})
vim.api.nvim_set_keymap('', '<Leader>mta', ':TestSuite<CR>', {})
vim.api.nvim_set_keymap('', '<Leader>mm', ':Dispatch! mix ecto.migrate<CR>', {})
vim.api.nvim_set_keymap('', '<Leader>ot', ':term<cr>a', {})

vim.api.nvim_set_keymap('i', '<C-s>', '<esc>:w<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-s>', ':w<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-p>', ':Files<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('', '<Esc><Esc>', ':noh<cr>', { silent = true })
vim.api.nvim_set_keymap('n', '<F2>', '<Plug>(coc-rename)', {})
vim.api.nvim_set_keymap('n', '<F5>', ':e!<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<F9>', ':NvimTreeFindFileToggle<CR>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<F10>', ':wqa<CR>', {})

-- Use <c-space> to trigger completion.
vim.api.nvim_set_keymap("i", "<C-Space>", "coc#refresh()", { silent = true, expr = true })

vim.g.spacevim_enabled_layers = { 'core/root', 'core/behavior', 'core/buffers', 'core/files', 'core/files/vim', 'core/lisp', 'core/quit', 'core/windows', 'core/zoom', 'git' }

vim.g.coc_global_extensions = {
  'coc-tsserver',
  'coc-eslint',
  'coc-go',
  'coc-stylelintplus',
  'coc-rust-analyzer',
  'coc-prettier',
  'coc-json',
  'coc-elixir',
  'coc-emmet',
  'coc-css',
  'coc-diagnostic',
  'coc-snippets',
  'coc-solargraph',
  'coc-deno'
}
vim.g.coc_snippet_next = '<C-n>'
vim.g.coc_snippet_prev = '<C-k>'

vim.g.mix_format_on_save = 1
vim.g.jsx_ext_required = 0
vim.g.airline_powerline_fonts = 0
vim.g.terraform_fmt_on_save = 1

-- Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
vim.g.UltiSnipsExpandTrigger = "<tab>"
vim.g.UltiSnipsJumpForwardTrigger = "<C-b>"
vim.g.UltiSnipsJumpBackwardTrigger = "<C-z>"
vim.g.UltiSnipsEditSplit = 'vertical'

vim.g.AutoPairs = {
  ['('] = ')',
  ['['] = ']',
  ['{'] = '}',
  ["`"] = "`",
  ['```'] = '```',
  ['"""'] = '"""'
}

vim.api.nvim_create_autocmd({'Filetype'}, {
  pattern = {'c', 'make', 'go', 'php', 'rust'},
  command = 'setlocal tabstop=4 shiftwidth=4 noexpandtab'
})

vim.api.nvim_create_autocmd({'Filetype'}, {
  pattern = {'swift'},
  command = 'setlocal tabstop=2 shiftwidth=2 expandtab'
})

vim.api.nvim_create_autocmd({'Filetype'}, {
  pattern = {'yaml'},
  command = 'setlocal inde='
})

vim.api.nvim_create_autocmd({'FocusGained', 'BufEnter', 'CursorHold', 'CursorHoldI'}, {
  command = ':checktime'
})

if vim.fn.has("unix") then
  local uname = vim.fn.system('uname')
  local linux = true
  local daytime = true
  if uname == "Darwin\n" then
    linux = false
    vim.g.python_host_prog = '/usr/local/bin/python2'
    vim.g.python3_host_prog = '/opt/homebrew/bin/python3'
    daytime = vim.fn.system("$HOME/.dotfiles/darkmode.Darwin") == "DAYTIME\n"
  else
    linux = true
    daytime = vim.fn.system("$HOME/.dotfiles/daytime") == "DAYTIME\n"
  end

  if not daytime then
    vim.cmd('colorscheme distinguished')
  else
    vim.cmd('colorscheme cobalt2')
  end
end

if vim.fn.has('unix') == 1 then
  if not vim.env.VIM_COLORSCHEME == "" then
    vim.cmd('colorscheme ' .. vim.env.VIM_COLORSCHEME)
  end
end

vim.g.mapleader = ' '

vim.api.nvim_set_keymap('n', '<Space>', '<Nop>', { noremap = true })

vim.api.nvim_create_autocmd({'BufRead','BufNewFile'}, {
  pattern = '*',
  command = 'set wrap linebreak nolist textwidth=0 wrapmargin=0'
})

vim.g.base16colorspace = 256

-- coc config
vim.opt.shortmess:append('c')
vim.opt.signcolumn = 'yes'

vim.api.nvim_set_keymap('i', '<F9>', '<esc>', { silent = true })
