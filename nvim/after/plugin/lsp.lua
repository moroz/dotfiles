-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(client, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  vim.keymap.set('n', '<F2>', vim.lsp.buf.rename, { noremap = true })
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', require('telescope.builtin').lsp_definitions, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
  nmap('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')

  require("lsp-format").on_attach(client, bufnr)
end

-- mason-lspconfig requires that these setup functions are called in this order
-- before setting up the servers.
require('mason').setup()
require('mason-lspconfig').setup()

local prettier = {
  formatCommand = [[prettier --stdin-filepath ${INPUT}]],
  formatStdin = true,
}

local terraformformat = {
  formatCommand = [[terraform fmt -]],
  formatStdin = true,
}

local prettierHTML = {
  formatCommand = [[echo "${INPUT}" | grep -iqe '\.html\.tmpl$' && prettier --parser html --stdin-filepath ${INPUT}]],
  formatStdin = true,
}

local goformat = {
  formatCommand = [[goimports]],
  formatStdin = true,
}

local elixirformat = {
  formatCommand = [[mix format -]],
  formatStdin = true,
}

local rubyformat = {
  formatCommand = [[rubocop -a -f fi -s ${INPUT} --stderr]],
  formatStdin = true,
}

local csharp_format = {
  formatCommand = [[dotnet csharpier --write-stdout --fast]],
  formatStdin = true,
}

local servers = {
  -- clangd = {},
  gopls = {},
  -- pyright = {},
  -- rust_analyzer = {},
  efm = {
    init_options = { documentFormatting = true },
    languages = {
      typescript = { prettier },
      typescriptreact = { prettier },
      json = { prettier },
      css = { prettier },
      scss = { prettier },
      javascript = { prettier },
      javascriptreact = { prettier },
      terraform = { terraformformat },
      gohtmltmpl = { prettierHTML },
      go = { goformat },
      elixir = { elixirformat },
      ruby = { rubyformat },
      cs = { csharp_format },
    }
  },
  tsserver = {},
  -- html = { filetypes = { 'html', 'twig', 'hbs' }, format = { templating = true } },
  -- terraformls = {},
  templ = {},
  elixirls = {},
  svelte = {},
  rubocop = {},

  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
}

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = { "gopls", "tsserver", "elixirls", "svelte", "templ", "efm", "cssls", "ruby_lsp", "lua_ls", "omnisharp" },
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
      filetypes = (servers[server_name] or {}).filetypes,
      init_options = (servers[server_name] or {}).init_options,
    }
  end,
}

vim.lsp.set_log_level("ERROR")
