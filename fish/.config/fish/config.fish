# Include executables in path
set -gx PATH ~/.bin $PATH
set -x PATH ~/.cargo/bin $PATH

set -x GOROOT (brew --prefix golang)/libexec
set -x GOPATH $HOME/go

set -x PATH $GOPATH/bin $PATH
set -x PATH $GOROOT/bin $PATH

set -gx EDITOR emacs

# Remove the welcome to fish message
set fish_greeting

if test -e ~/exports.local
  source ~/exports.local
end

# Fixes fish not loading functions for some reason. See
# https://github.com/fish-shell/fish-shell/issues/1915
for i in (functions);functions $i > /dev/null; end

# Enable starship
# https://github.com/starship/starship
# if type -q starship
#     eval (starship init fish)
# end

# Enable kitty completions
if type -q kitty
    kitty + complete setup fish | source
end
