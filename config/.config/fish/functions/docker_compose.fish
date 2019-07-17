function dcB
  docker-compose build --no-cache $argv
end

function dcd
  docker-compose down $argv
end

function dce
  docker-compose exec $argv
end

function dck
  docker-compose kill $argv
end

function dcl
  docker-compose logs $argv
end

function dcL
  docker-compose logs -f $argv
end

function dcps
  docker-compose ps $argv
end

function dcp
  docker-compose pause $argv
end

function dcP
  docker-compose unpause $argv
end

function dcpl
  docker-compose pull $argv
end

function dcph
  docker-compose push $argv
end

function dcr
  docker-compose run $argv
end

function dcR
  docker-compose restart $argv
end

function dcrm
  docker-compose rm $argv
end

function dcs
  docker-compose start $argv
end

function dcS
  docker-compose stop $argv
end

function dcsc
  docker-compose scale $argv
end

function dcu
  docker-compose up $argv
end

function dcU
  docker-compose up -d $argv
end

function dcv
  docker-compose version $argv
end

