source "$HOME/.zsh/functions"
source "$HOME/.zsh/aliases"
source "$HOME/.zsh/exports"

autoload -Uz compinit
compinit

# ensure dotfiles bin directory is loaded first
PATH=$HOME/.bin:/usr/local/sbin:$PATH
PATH=$HOME/.cargo/bin:$PATH
PATH=$PATH:/usr/local/go/bin

eval "$(starship init zsh)"

source <(antibody init)
antibody bundle < ~/.zsh_plugins.txt
[[ /usr/local/bin/kubectl ]] && source <(kubectl completion zsh)
