# Install Fundle if not present
if not functions -q fundle
    eval (curl -sfL https://git.io/fundle-install)
end

fundle plugin acomagu/fish-async-prompt
fundle plugin edc/bass
fundle plugin gazorby/fish-abbreviation-tips
fundle plugin jethrokuan/z
fundle plugin jorgebucaran/fish-bax
fundle plugin joseluisq/gitnow
fundle plugin markcial/upto
fundle plugin oh-my-fish/plugin-pj

fundle init

# Aliases
alias lzg="lazygit"
alias help="tldr"
alias ping="prettyping --nolegend"
alias k="kubectl"
alias pn="pnpm"

# Path aliases
alias ..="cd ../"
alias ...="cd ../../"
alias ....="cd ../../../"
alias .....="cd ../../../../"
alias ......="cd ../../../../../"

# Git aliases
alias g="git"
alias gb="git branch"
alias gc="git commit --verbose"
alias gco="git checkout"
alias gf="git fetch"
alias gl="git log --all --decorate --oneline --graph"
alias gr="git rebase"
alias gp="git pull"

thefuck --alias | source

# 1Password completions
op completion fish | source

# Add local scripts to PATH
set PATH ~/.bin $PATH

# Add go to PATH
set PATH (go env GOPATH)/bin $PATH

# Add cargo crates to PATH
set PATH ~/.cargo/bin $PATH

# Add `cape` to PATH
set PATH ~/projects/cli $PATH

# Init the startship prompt
starship init fish | source

# Disable the startup message
set -e fish_greeting

if test -e ~/exports.local.fish
    source ~/exports.local.fish
end
