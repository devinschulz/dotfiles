source "$HOME/.zsh/functions"
source "$HOME/.zsh/aliases"
source "$HOME/.zsh/exports"

autoload -Uz compinit
compinit

# ensure dotfiles bin directory is loaded first
PATH="$HOME/.bin:/usr/local/sbin:$PATH"
PATH="$HOME/.cargo/bin:$PATH"

# Kitty completions
if [ -x "$(command -v kitty)" ]; then
  kitty + complete setup zsh | source /dev/stdin
fi

source <(antibody init)
antibody bundle < ~/.zsh_plugins.txt
