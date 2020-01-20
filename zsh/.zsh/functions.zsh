# Make a directory and then enter it
mkcd() {
  mkdir -p $1
  cd $1
}

# Set the tab title to the current working directory within a terminal
precmd() {
  echo -ne "\e]1;${PWD##*/}\a"
}

# Remove all local git branches which have already been merged in.
git-cleanup() {
  git branch --merged | egrep -v "(^\*|master|dev)" | xargs git branch -d
}
