set -e
cabal run main
hp2ps -e8in -c main.hp
vim main.prof
