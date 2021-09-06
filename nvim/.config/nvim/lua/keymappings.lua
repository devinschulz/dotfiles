local utils = require "utils"

local opts = {
  nnoremap = { noremap = true, silent = true },
  inoremap = { noremap = true, silent = true },
  vnoremap = { noremap = true, silent = true },
  xnoremap = { noremap = true, silent = true },
  tnoremap = { noremap = true, silent = true },
}

local default_keys = {
  insert_mode = { },

  normal_mode = {
    -- Better window movement
    { "<C-h>", "<C-w>h" },
    { "<C-j>", "<C-w>j" },
    { "<C-k>", "<C-w>k" },
    { "<C-l>", "<C-w>l" },

    -- Resize with arrows
    { "<C-Up>", ":resize -2<CR>" },
    { "<C-Down>", ":resize +2<CR>" },
    { "<C-Left>", ":vertical resize -2<CR>" },
    { "<C-Right>", ":vertical resize +2<CR>" },

    { "<F5>", ":so %<CR>" },  -- source file
    { "<space>", "za" },  -- easy folding

    { "|", "<C-W><C-V>" },  -- easy vsplit
    { "_", "<C-W><C-S>" },  -- easy hsplit

    { "<C-q>", "<C-W><C-Q>" },  -- easy quit
    { "<Leader>w", ":w!<cr>" },

    -- Telescope
    { "<C-p>", "<cmd>lua require('telescope.builtin').find_files()<cr>" },
    { "<leader>gg", "<cmd>lua require('telescope.builtin').live_grep()<cr>" },
    { "<leader>rr", "<cmd>lua require('telescope.builtin').oldfiles()<cr>"},

    -- NvimTree
    { "<leader>t", ":NvimTreeToggle<CR>" },
    -- { "<leader>t", "<cmd>lua require('config.nvimtree').toggle_tree()<cr>" },
    { "<leader>ff", ":NvimTreeFindFile<CR>"},
    -- { "<leader>ff", "<cmd>lua require('config.nvimtree').find_file()<cr>" },

    -- nvim-bufferline
    { '<leader><leader>', ':BufferLineCycleNext<CR>' },
    { '<leader>bd', ':BufferLinePickClose<CR>' },
    { '<leader>1', '<cmd>BufferLineGoToBuffer 1<CR>'},
    { '<leader>2', '<cmd>BufferLineGoToBuffer 2<CR>'},
    { '<leader>3', '<cmd>BufferLineGoToBuffer 3<CR>'},
    { '<leader>4', '<cmd>BufferLineGoToBuffer 4<CR>'},
    { '<leader>5', '<cmd>BufferLineGoToBuffer 5<CR>'},
    { '<leader>6', '<cmd>BufferLineGoToBuffer 6<CR>'},
    { '<leader>7', '<cmd>BufferLineGoToBuffer 7<CR>'},
    { '<leader>8', '<cmd>BufferLineGoToBuffer 8<CR>'},
    { '<leader>9', '<cmd>BufferLineGoToBuffer 9<CR>'},

  },

  term_mode = {
    -- Terminal window navigation
    { "<C-h>", "<C-\\><C-N><C-w>h" },
    { "<C-j>", "<C-\\><C-N><C-w>j" },
    { "<C-k>", "<C-\\><C-N><C-w>k" },
    { "<C-l>", "<C-\\><C-N><C-w>l" },

    -- Terminal resize
    { "<Tab>k", "<C-\\><C-N>z70<CR>i" },
    { "<Tab>j", "<C-\\><C-N>z20<CR>i" },

  },

  visual_mode = {
    -- Better indenting\
    { "<", "<gv" },
    { ">", ">gv" },

  },

  visual_block_mode = {
    -- Move selected line / block of text in visual mode
    { "K", ":move '<-2<CR>gv-gv" },
    { "J", ":move '>+1<CR>gv-gv" },

  },
}

utils.add_keymap_normal_mode(opts.nnoremap, default_keys["normal_mode"])
utils.add_keymap_insert_mode(opts.inoremap, default_keys["insert_mode"])
utils.add_keymap_visual_mode(opts.vnoremap, default_keys["visual_mode"])
utils.add_keymap_visual_block_mode(opts.xnoremap, default_keys["visual_block_mode"])
utils.add_keymap_term_mode(opts.tnoremap , default_keys["term_mode"])
