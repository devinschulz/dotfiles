local status_ok, feline = pcall(require, "feline")
if not status_ok then return end

local tokyonight = {
  fg = "#c8d3f5",
  bg = "#1e2030",
  bg_dark = "#222436",
  green = "#c3e88d",
  yellow = "#ffc777",
  purple = "#fca7ea",
  orange = "#d19a66",
  fg_dark = "#a9b1d6",
  red = "#ff757f",
  blue = "#82aaff",
  dark_red = "#c53b53",
}

feline.setup {
  theme = tokyonight
}

feline.winbar.setup {}
