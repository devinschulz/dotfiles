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

# Git aliases
alias g="git"
alias gb="git branch"
alias gc="git commit --verbose"
alias gco="git checkout"
alias gf="git fetch"
alias gl="git log --all --decorate --oneline --graph"
alias gr="git rebase"

thefuck --alias | source

# Add local scripts to PATH
set PATH ~/.bin $PATH

# Add go to PATH
set PATH (go env GOPATH)/bin $PATH

# Add cargo crates to PATH
set PATH ~/.cargo/bin $PATH

# Add the coordinator to PATH
set PATH ~/projects/coordinator/bin $PATH

# Init the startship prompt
starship init fish | source

# Disable the startup message
set -e fish_greeting
