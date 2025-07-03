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

# Add local scripts to PATH
set PATH ~/.bin $PATH
set PATH ~/.local/bin $PATH

# Add go to PATH
set PATH (go env GOPATH)/bin $PATH

# Add cargo crates to PATH
set PATH ~/.cargo/bin $PATH

# Add `cape` to PATH
set PATH ~/projects/cli $PATH

# Set the default editor to helix
set -x EDITOR hx

set -Ux PYENV_ROOT $HOME/.pyenv
set -U fish_user_paths $PYENV_ROOT/bin $fish_user_paths

pyenv init - fish | source

# Init the startship prompt
starship init fish | source

# Disable the startup message
set fish_greeting

if test -e ~/exports.local.fish
    source ~/exports.local.fish
end
