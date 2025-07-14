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

vim.g.user_emmet_install_global = 0
vim.g.user_emmet_settings = {
  templ = {
    extends = "html"
  },
  heex = {
    extends = "html"
  },
  astro = {
    extends = "html"
  },
  eruby = {
    extends = "html"
  },
  svelte = {
    extends = "html"
  },
}

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

if os.getenv('NO_SYNTAX') == 'true' then
  vim.cmd.syntax('off')
end

local function is_wsl()
  local output = vim.fn.system('uname -r')
  return output:lower():match('microsoft') ~= nil
end

if is_wsl() then
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

local function has_dark_mode()
  local uname = vim.loop.os_uname().sysname
  return uname == "Darwin" or uname == "Linux"
end

local function is_day()
  local uname = vim.loop.os_uname().sysname
  if uname == "Darwin" then
    local output = vim.fn.system("~/.dotfiles/darkmode.Darwin")
    return output == "DAYTIME\n"
  elseif uname == "Linux" then
    local output = vim.fn.system("dconf read /org/gnome/desktop/interface/color-scheme")
    return output == "'prefer-light'\n"
  end
end

if has_dark_mode() then
  if is_day() then
    vim.cmd.colorscheme(os.getenv('VIM_LIGHT_COLORSCHEME') or 'tempus_fugit')
  else
    vim.cmd.colorscheme(os.getenv('VIM_DARK_COLORSCHEME') or 'tempus_winter')
  end
else
  vim.cmd.colorscheme(os.getenv('VIM_COLORSCHEME') or 'distinguished')
end
