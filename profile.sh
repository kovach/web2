set -e
stack build --profile --force-dirty --ghc-options=-fprof-auto-top arrow:arrow-profile
stack exec -- arrow-profile
#hp2ps -e8in -c main.hp
vim arrow-profile.prof
