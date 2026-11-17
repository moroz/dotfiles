-- debug.lua
--
-- Debugging with nvim-dap, set up for Go (delve via nvim-dap-go).
--
-- delve comes from mise (`go:github.com/go-delve/delve/cmd/dlv` in the global
-- mise config), so Mason does not install it.
--
-- Keys
--   <F12>  start debugging, or continue when a session is running.
--          In a *_test.go buffer it debugs the test under the cursor;
--          elsewhere it builds and runs the package at the repository root
--          (what `<F12>` did with vim-delve).
--   <F8>   toggle breakpoint (also from vim-delve)
--   <F6>   step over      <F7> step into      <F4> step out
--   <leader>d...  everything else; which-key lists it under [D]ebug.
--
-- <F2>, <F5>, <F9> and <F10> are taken (LSP rename, :lsp restart, Neo-tree,
-- :wqa!), so the stepping keys avoid them.

-- Root of the Go project the current buffer belongs to: the repository root,
-- else the nearest go.mod, else the working directory.
local function project_root()
  return vim.fs.root(0, { '.git', '.jj' }) or vim.fs.root(0, 'go.mod') or vim.fn.getcwd()
end

local function start_or_continue()
  local dap = require 'dap'
  if dap.session() then
    dap.continue()
  elseif vim.api.nvim_buf_get_name(0):match '_test%.go$' then
    require('dap-go').debug_test()
  else
    local root = project_root()
    dap.run {
      type = 'go',
      name = 'Debug ' .. vim.fn.fnamemodify(root, ':t'),
      request = 'launch',
      program = root,
      cwd = root,
    }
  end
end

local function dap_call(fn, ...)
  local args = { ... }
  return function()
    require('dap')[fn](unpack(args))
  end
end

return {
  'mfussenegger/nvim-dap',
  dependencies = {
    'rcarriga/nvim-dap-ui',
    'nvim-neotest/nvim-nio',
    'theHamsta/nvim-dap-virtual-text',
    'leoluz/nvim-dap-go',
  },
  keys = {
    -- Function keys for the moves you repeat while stepping
    { '<F12>', start_or_continue, desc = 'Debug: Start/Continue' },
    { '<F8>', dap_call 'toggle_breakpoint', desc = 'Debug: Toggle breakpoint' },
    { '<F6>', dap_call 'step_over', desc = 'Debug: Step over' },
    { '<F7>', dap_call 'step_into', desc = 'Debug: Step into' },
    { '<F4>', dap_call 'step_out', desc = 'Debug: Step out' },

    -- Session
    { '<leader>dc', start_or_continue, desc = 'Debug: [C]ontinue / start' },
    { '<leader>dC', dap_call 'continue', desc = 'Debug: Pick [C]onfiguration' },
    { '<leader>dl', dap_call 'run_last', desc = 'Debug: Run [L]ast' },
    { '<leader>dq', dap_call 'terminate', desc = 'Debug: [Q]uit session' },
    { '<leader>dp', dap_call 'pause', desc = 'Debug: [P]ause' },
    { '<leader>dr', dap_call 'restart', desc = 'Debug: [R]estart' },

    -- Stepping
    { '<leader>dn', dap_call 'step_over', desc = 'Debug: [N]ext (step over)' },
    { '<leader>di', dap_call 'step_into', desc = 'Debug: Step [I]nto' },
    { '<leader>do', dap_call 'step_out', desc = 'Debug: Step [O]ut' },
    { '<leader>dg', dap_call 'run_to_cursor', desc = 'Debug: [G]o to cursor' },
    { '<leader>dk', dap_call 'up', desc = 'Debug: Stack frame up' },
    { '<leader>dj', dap_call 'down', desc = 'Debug: Stack frame down' },
    { '<leader>df', dap_call 'focus_frame', desc = 'Debug: [F]ocus current frame' },

    -- Breakpoints
    { '<leader>db', dap_call 'toggle_breakpoint', desc = 'Debug: Toggle [B]reakpoint' },
    {
      '<leader>dB',
      function()
        require('dap').set_breakpoint(vim.fn.input 'Breakpoint condition: ')
      end,
      desc = 'Debug: Conditional [B]reakpoint',
    },
    {
      '<leader>dL',
      function()
        require('dap').set_breakpoint(nil, nil, vim.fn.input 'Log message ({expr} interpolates): ')
      end,
      desc = 'Debug: [L]og point',
    },
    { '<leader>dx', dap_call 'clear_breakpoints', desc = 'Debug: Clear all breakpoints' },
    { '<leader>da', dap_call 'list_breakpoints', desc = 'Debug: List [A]ll breakpoints' },

    -- Go tests
    {
      '<leader>dt',
      function()
        require('dap-go').debug_test()
      end,
      desc = 'Debug: Nearest [T]est',
    },
    {
      '<leader>dT',
      function()
        require('dap-go').debug_last_test()
      end,
      desc = 'Debug: Last [T]est',
    },

    -- Inspecting
    {
      '<leader>de',
      function()
        require('dapui').eval(nil, { enter = true })
      end,
      mode = { 'n', 'v' },
      desc = 'Debug: [E]valuate expression under cursor / selection',
    },
    {
      '<leader>dE',
      function()
        require('dapui').eval(vim.fn.input 'Evaluate: ', { enter = true })
      end,
      desc = 'Debug: [E]valuate typed expression',
    },
    {
      '<leader>du',
      function()
        require('dapui').toggle()
      end,
      desc = 'Debug: Toggle [U]I',
    },
    { '<leader>dR', function() require('dap').repl.toggle() end, desc = 'Debug: Toggle [R]EPL' },
  },
  config = function()
    local dap = require 'dap'
    local dapui = require 'dapui'

    -- Sidebar with scopes, stack, breakpoints and watches; console and REPL
    -- below. The REPL window carries clickable controls (play, step into,
    -- step over, step out, restart, stop).
    dapui.setup {
      icons = { expanded = '▾', collapsed = '▸', current_frame = '*' },
      controls = {
        enabled = true,
        element = 'repl',
        icons = vim.g.have_nerd_font and {} or {
          pause = '⏸',
          play = '▶',
          step_into = '⏎',
          step_over = '⏭',
          step_out = '⏮',
          step_back = 'b',
          run_last = '▶▶',
          terminate = '⏹',
          disconnect = '⏏',
        },
      },
      layouts = {
        {
          position = 'left',
          size = 45,
          elements = {
            { id = 'scopes', size = 0.45 },
            { id = 'stacks', size = 0.25 },
            { id = 'watches', size = 0.15 },
            { id = 'breakpoints', size = 0.15 },
          },
        },
        {
          position = 'bottom',
          size = 12,
          elements = {
            { id = 'repl', size = 0.5 },
            { id = 'console', size = 0.5 },
          },
        },
      },
    }

    -- Values of variables shown at the end of the line while stopped. 'eol'
    -- rather than the default 'inline', which splices values into the code
    -- and wraps long ones over several lines.
    require('nvim-dap-virtual-text').setup { virt_text_pos = 'eol' }

    vim.api.nvim_set_hl(0, 'DapBreak', { fg = '#e51400' })
    vim.api.nvim_set_hl(0, 'DapStop', { fg = '#ffcc00' })
    vim.api.nvim_set_hl(0, 'DapStoppedLine', { link = 'Visual' })
    local breakpoint_icons = vim.g.have_nerd_font
        and { Breakpoint = '', BreakpointCondition = '', BreakpointRejected = '', LogPoint = '', Stopped = '' }
      or { Breakpoint = '●', BreakpointCondition = '⊜', BreakpointRejected = '⊘', LogPoint = '◆', Stopped = '▶' }
    for type, icon in pairs(breakpoint_icons) do
      local tp = 'Dap' .. type
      local hl = (type == 'Stopped') and 'DapStop' or 'DapBreak'
      vim.fn.sign_define(tp, { text = icon, texthl = hl, numhl = hl, linehl = (type == 'Stopped') and 'DapStoppedLine' or nil })
    end

    -- Open the UI when a session starts; leave it open when the program ends
    -- so its output stays readable. <leader>du closes it.
    dap.listeners.after.event_initialized['dapui_config'] = dapui.open

    require('dap-go').setup {
      delve = {
        -- On Windows delve must be run attached or it crashes.
        detached = vim.fn.has 'win32' == 0,
      },
    }
  end,
}
