#! /bin/bash

function build {
  cabal build ambar-hs-utils -j "${@}"
}

function test {
  cabal run ambar-hs-utils-tests -- "${@}"
}

function typecheck {
  # Start fast type-checking of the library. (Everything but Main.hs)
  # Watches your files and type-checks on save
  ghcid -c 'cabal v2-repl' ambar-hs-utils "${@}"
}

function typecheck-tests {
  # Start fast type-checking of the executable. (Just Main.hs)
  # Watches your files and type-checks on save
  ghcid -c 'cabal v2-repl' ambar-hs-utils-tests "${@}"
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
