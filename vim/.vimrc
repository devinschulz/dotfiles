let configs = [
  \ "plugins",
  \ "plugin-settings",
  \ "general",
  \ "display",
  \ "backups",
  \ "commands",
  \ "mappings",
  \ "search",
  \ ]

for file in configs
  let x = expand("~/.vim/".file.".vim")
  if filereadable(x)
    execute 'source' x
  endif
endfor
