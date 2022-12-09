vim.api.nvim_notify("trying to load neotest")

local status_ok, neotest = pcall(require, "neotest")
if not status_ok then
  vim.api.nvim_echo("neotest not loaded", status_ok)
  return
end

vim.api.nvim_notify("neotest loaded")

neotest.setup({
  adapters = {
    require("neotest-plenary"),
    require("neotest-jest")({
      jestCommand = "yarn test --",
      jestConfigFile = "jest.config.ts",
      env = { CI = true },
      cwd = function()
        return vim.fn.getcwd()
      end,
    }),
    require("neotest-rust"),
    require("neotest-deno"),
  },
})

-- local function bind_key(key, dlg)
--   vim.keymap.set("n", "<Leader>n" .. key, dlg)
-- end
--
-- bind_key("t", neotest.run.run)
-- bind_key("T", function()
--   neotest.run.run(vim.fn.expand("%"))
-- end)
-- bind_key("<C-t>", function()
--   neotest.run.run_last()
-- end)
-- bind_key("o", neotest.output.open)
-- bind_key("p", neotest.output_panel.toggle)
--
-- bind_key("d", function()
--   neotest.run.run({ strategy = "dap" })
-- end)
-- bind_key("D", function()
--   neotest.run.run({ vim.fn.expand("%"), strategy = "dap" })
-- end)
-- bind_key("<C-d>", function()
--   neotest.run.run_last({ strategy = "dap" })
-- end)
--
-- bind_key("k", neotest.run.stop)
-- bind_key("s", neotest.summary.toggle)

-- vim.api.nvim_create_user_command("TestSummary", neotest.summary.toggle, {})
-- vim.api.nvim_create_user_command("TestNearest", neotest.run.run, {})
-- vim.api.nvim_create_user_command("TestDap", function()
--   neotest.run.run({ strategy = "dap" })
-- end, {})
-- vim.api.nvim_create_user_command("TestLast", neotest.run.run_last, {})
-- vim.api.nvim_create_user_command("TestLastDap", function()
--   neotest.run.run_last({ strategy = "dap" })
-- end, {})
-- vim.api.nvim_create_user_command("TestStop", function()
--   neotest.run.stop({})
-- end, {})
-- vim.api.nvim_create_user_command("TestFile", function()
--   neotest.run.run(vim.fn.expand("%"))
-- end, {})
-- vim.api.nvim_create_user_command("TestOutput", function()
--   neotest.output.open({ short = false, enter = true })
-- end, {})
-- vim.api.nvim_create_user_command("TestDap", function()
--   neotest.run.run({ strategy = "dap" })
-- end, {})
--
-- local opts = { noremap = false, silent = true }
-- vim.keymap.set("n", "<space>ts", neotest.summary.toggle, opts)
-- vim.keymap.set("n", "<space>tl", neotest.run.run_last, opts)
-- vim.keymap.set("n", "<space>tr", neotest.run.run, opts)
-- vim.keymap.set("n", "<space>to", function()
--   neotest.output.open({ short = false, enter = true })
-- end, opts)
--
-- vim.keymap.set("n", "]n", function()
--   neotest.jump.next({ status = "failed" })
-- end, opts)
-- vim.keymap.set("n", "[n", function()
--   neotest.jump.prev({ status = "failed" })
-- end, opts)
--
-- local neotest_group = vim.api.nvim_create_augroup("neotest", {})

-- local autocmd_id
-- vim.api.nvim_create_user_command("TestWatch", function()
--   autocmd_id = vim.api.nvim_create_autocmd(
--     "BufWritePost",
--     { command = "TestLast", group = neotest_group, pattern = "**/*.dart" }
--   )
-- end, {})
--
-- vim.api.nvim_create_user_command("TestWatchStop", function()
--   vim.api.nvim_del_autocmd(autocmd_id)
-- end, {})
