function dc
  docker-compose $argv
end

function dcb
  dc build $argv
end

function dcB
  dc build --no-cache $argv
end

function dcd
  dc down $argv
end

function dce
  dc exec $argv
end

function dck
  dc kill $argv
end

function dcl
  dc logs $argv
end

function dcp
  dc pause $argv
end

function dcpl
  dc pull $argv
end

function dcph
  dc push $argv
end

function dcps
  dc ps $argv
end

function dcr
  dc run $argv
end

function dcR
  dc run --rm $argv
end

function dcrm
  dc rm $argv
end

function dcs
  dc restart $argv
end

function dcu
  dc up $argv
end

function dcU
  dc up -d $argv
end

function dcv
  dc version $argv
end

function dcs
  dc stop $argv
end
