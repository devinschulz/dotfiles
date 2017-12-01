# load all custom executable functions
for function in ~/dotfiles/zsh/functions/*; do
  source $function
done

[[ -f ~/dotfiles/aliases ]] && source ~/dotfiles/aliases
[[ -f ~/dotfiles/zsh/exports.zsh ]] && source ~/dotfiles/zsh/exports.zsh

# Load a local zshrc file if it exists
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

. ~/dotfiles/z/z.sh

source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
