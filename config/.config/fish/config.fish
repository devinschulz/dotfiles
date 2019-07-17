# Include executables in path
set -gx PATH $PATH ~/.bin
set -gx PATH $PATH ~/.cargo/bin

set -gx EDITOR nvim

# Remove the welcome to fish message
set fish_greeting

if test -e ~/exports.local
  source ~/exports.local
end

# Fixes fish not loading functions for some reason. See
# https://github.com/fish-shell/fish-shell/issues/1915
for i in (functions);functions $i > /dev/null; end

