local ls = require 'luasnip'
local s = ls.snippet
local i = ls.insert_node
local f = ls.function_node
local fmt = require('luasnip.extras.fmt').fmt

-- Capitalizes the buffer's filename for use as the component name.
local function filename_to_component()
  return function()
    local name = vim.fn.expand '%:t:r'
    name = name:gsub('^%l', string.upper)
    return name
  end
end

return {
  s(
    'rfc',
    fmt(
      [[
      export function {}({{ {} }}) {{
        return (
          <div>
            {}
          </div>
        )
      }}
      ]],
      {
        f(filename_to_component()),
        i(1),
        i(0),
      }
    )
  ),
}
