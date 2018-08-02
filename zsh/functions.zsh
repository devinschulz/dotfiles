# Make a directory and then enter it
mkcd() {
  mkdir -p $1
  cd $1
}

# Set the tab title to the current working directory within a terminal
precmd() {
  echo -ne "\e]1;${PWD##*/}\a"
}

# Docker

# Start the container in detached mode
dcu() {
  [[ -z "$1" ]] && docker-compose up -d $1 || docker-compose up -d
}

# Build the container
dcb() {
  [[ -z "$1" ]] && docker-compose build $1 || docker-compose build
}

# Stop the container
dcs() {
  [[ -z "$1" ]] && docker-compose stop $1 || docker-compose stop
}

# Restart the container
# Performs a restart by calling stop and then start. I have ran into issues
# whenever I only call docker-compse restart.
dcr() {
  [[ -z "$1" ]] && dcs $1 && dcu $1 || dcs && dcu
}

# Stop and remove the container
dcrm() {
  [[ -z "$1" ]] && dcs $1 && docker-compose rm -f $1 || dcs && docker-compose rm -f
}

# Tail the logs of a container
dcl() {
  [[ -z "$1" ]] && docker-compose logs -f $1 || docker-compose logs -f
}
