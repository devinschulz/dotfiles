[[ -f ~/dotfiles/aliases ]] && source ~/dotfiles/aliases
[[ -f ~/dotfiles/zsh/exports.zsh ]] && source ~/dotfiles/zsh/exports.zsh
[[ -f ~/dotfiles/zsh/functions.zsh ]] && source ~/dotfiles/zsh/functions.zsh

# Load a local zshrc file if it exists
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

. ~/dotfiles/zsh/plugins/z/z.sh

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi
