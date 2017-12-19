export VISUAL=vim
export EDITOR=$VISUAL

export GITHUB_USER=devinschulz

export GOPATH=$HOME/go
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

[[ -f ~/exports.local ]] && source ~/exports.local
