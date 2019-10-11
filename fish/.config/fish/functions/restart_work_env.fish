function restart_work_env
  osascript -e 'tell application "Docker" to quit'
  killall Docker
  open -a Docker

  echo Waiting for Docker to boot

  set seconds 0

  while ! docker system info >/dev/null 2>&1
    echo -ne Docker is not ready, waiting... Elapsed time (math "$seconds * 5") seconds\r
    sleep 5
    set seconds (math $seconds + 1)
  end

  echo Docker is active and ready

  start_work_env
end

function start_work_env
  cd ~/go/src/github.com/InVisionApp/conversations-ui
  echo "Starting conversations-ui"
  docker-compose up -d --force-recreate
  cd -

  cd ~/InVision/trapezoid-console
  echo "Starting trapezoid-console"
  docker-compose up -d
  cd -

  cd ~/InVision/console-ui-v7
  echo "Starting console-ui-v7"
  docker-compose up -d
  cd -

  cd ~/InVision/invision-local
  echo "Starting invision-local"
  docker-compose up -d
  cd -
end
