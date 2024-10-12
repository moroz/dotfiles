vim.api.nvim_create_augroup("AutoFormat", {})

vim.api.nvim_create_autocmd(
    "BufWritePost",
    {
        pattern = "*.templ",
        group = "AutoFormat",
        callback = function()
            vim.cmd("silent !templ fmt %")
            vim.cmd("edit")
        end,
    }
)
