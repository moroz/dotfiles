vim.opt.nu = true
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
vim.o.smartcase = true
vim.opt.lbr = true
vim.opt.smartindent = true
vim.opt.eol = true
vim.opt.autoread = true
vim.opt.foldenable = false
vim.opt.guicursor = ""
vim.opt.updatetime = 300
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.wo.signcolumn = 'yes'
vim.o.fileformat = 'unix'

vim.opt.termguicolors = true

vim.g.mapleader = ' '

vim.g.neoformat_only_msg_on_error = true

vim.g.vim_svelte_plugin_use_typescript = 1
vim.g.vim_svelte_plugin_use_sass = 1
-- vim.g.go_fmt_autosave = 0

-- Set highlight on search
vim.o.hlsearch = false

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

local lightscheme = os.getenv('VIM_LIGHT_COLORSCHEME') or 'modus_operandi'
local darkscheme = os.getenv('VIM_DARK_COLORSCHEME') or 'modus_vivendi'

local preferred = ''

local is_wsl = function()
  local output = vim.fn.system('uname -r')
  return output:lower():match('microsoft') ~= nil
end

if jit.os == "Linux" then
  preferred = vim.fn.system('dconf read /org/gnome/desktop/interface/color-scheme'):gsub("%s+", "")
end

if jit.os == "OSX" then
  preferred = vim.fn.system("~/.dotfiles/darkmode.Darwin"):gsub("%s+", "")
end

if preferred == "'prefer-light'" or preferred == "DAYTIME" then
  vim.cmd.colorscheme(lightscheme)
else
  vim.cmd.colorscheme(darkscheme)
end

if os.getenv('NO_SYNTAX') == 'true' then
  vim.cmd.syntax('off')
end

if vim.fn.has('wsl') and jit.os ~= "OSX" then
  vim.g.clipboard = {
    name = 'WslClipboard',
    copy = {
      ['+'] = 'clip.exe',
      ['*'] = 'clip.exe',
    },
    paste = {
      ['+'] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
      ['*'] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
    },
    cache_enabled = 0,
  }
end
