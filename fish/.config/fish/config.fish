# Install Fundle if not present
if not functions -q fundle
  eval (curl -sfL https://git.io/fundle-install)
end

fundle plugin jethrokuan/z
fundle plugin franciscolourenco/done
fundle plugin jorgebucaran/fish-bax
fundle plugin markcial/upto
fundle plugin oh-my-fish/plugin-pj

fundle init

# Aliases
alias lzg="lazygit"
alias help="tldr"
alias ping="prettyping --nolegend"
alias g="git"

thefuck --alias | source

# Init the startship prompt
starship init fish | source
