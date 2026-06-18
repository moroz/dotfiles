-- You can add your own plugins here or in other files in this directory!
--  I promise not to create any merge conflicts in this directory :)
--
-- See the kickstart.nvim README for more information
return {
  {
    'evanleck/vim-svelte',
    branch = 'main',
    ft = 'svelte',
    dependencies = {
      'othree/html5.vim',
      'pangloss/vim-javascript',
    },
  },
}
