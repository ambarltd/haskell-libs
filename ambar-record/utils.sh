#! /bin/bash

function build {
  cabal build ambar-record -j "${@}"
}

function build-docs {
  cabal haddock
}

function test {
  cabal run ambar-record-tests -- "${@}"
}

function typecheck {
  # Start fast type-checking of the library. (Everything but Main.hs)
  # Watches your files and type-checks on save
  ghcid -c 'cabal v2-repl' ambar-record "${@}"
}

function typecheck-tests {
  # Start fast type-checking of the executable. (Just Main.hs)
  # Watches your files and type-checks on save
  ghcid -c 'cabal v2-repl' ambar-record-tests "${@}"
}

# If the first argument is a function run it.
if [[ $(type -t $1) == function ]];
  then
   $1 "${@:2}";
  else
    echo "Development utilities"
    echo ""
    echo "  ./util.sh COMMAND"
    echo ""
fi
