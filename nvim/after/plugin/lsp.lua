-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(server_name)
  return function(client, bufnr)
    local nmap = function(keys, func, desc)
      if desc then
        desc = 'LSP: ' .. desc
      end

      vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
    end

    vim.keymap.set('n', '<F2>', vim.lsp.buf.rename, { noremap = true })
    nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

    nmap('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')
    nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
    nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

    -- See `:help K` for why this keymap
    nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
    nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

    -- client.server_capabilities.semanticTokensProvider = nil

    local enable_format_lsps = { 'svelte', 'lua_ls', }
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

-- mason-lspconfig requires that these setup functions are called in this order
-- before setting up the servers.
require('mason').setup()
require('mason-lspconfig').setup()

local servers = {
  gopls = {},
  ts_ls = {},
  templ = {},
  elixirls = {},
  svelte = {},
  rubocop = {},
  tailwindcss = {
    filetypes = {
      "astro", "astro-markdown", "eelixir", "erb", "eruby", "gohtml", "haml", "html", "html-eex", "markdown",
      "mdx", "css", "sass", "scss", "javascript", "javascriptreact", "typescript", "typescriptreact",
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

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Restrict offsetEncoding to 'utf-16' to suppress `warning: multiple different client offset_encodings detected`
-- in clangd
capabilities['offsetEncoding'] = 'utf-16'

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  -- ensure_installed = { "ts_ls", "svelte", "templ", "lua_ls", "gopls", "elixirls", "efm" },
  ensure_installed = { "ts_ls", "svelte", "gopls", "elixirls", "clangd", "tailwindcss", "templ", "powershell_es", "rubocop", "solargraph", "astro" },
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

vim.lsp.set_log_level("ERROR")

vim.g.copilot_enabled = false
