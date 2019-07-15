function g
  hub $avgv
end

# Branch (b)

function gb
  g branch
end

function gba
  g branch --all --verbose
end

# Commit (c)

function gc
  g commit --verbose $argv
end

function gca
  gc --all $argv
end

function gcm
  gc -m $argv
end

function gcS
  gc -S $argv
end


# Fetch (f)

function gf
  g fetch $argv
end

function gfa
  gf --all $argv
end

function gfc
  g clone $argv
end

function gfcr
  g clone --recurse-submodules $argv
end

# Log (l)
function gl
  g log --topo-order --pretty=format:"${_git_log_medium_format}"
end


# Merge (m)
function gm
  g merge $argv
end

# Push (p)
function gp
  g push $argv
end

function gpf
  gp --force-with-lease
end

function gpF
  gp --force
end

function gpa
  gp --all
end

function gpA
  gp --all
  gp --tags
end

function gpt
  gp --tags
end

function gpc
  gp --set-upstream origin "$(git-branch-current 2> /dev/null)"
end

function gpp
  g pull origin "$(git-branch-current 2> /dev/null)"
  g push origin "$(git-branch-current 2> /dev/null)"
end
