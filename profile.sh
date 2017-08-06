set -e
stack build --profile --ghc-options=-fprof-auto arrow:arrow-profile
#stack build --profile --force-dirty --ghc-options=-fprof-auto arrow:arrow-profile
#stack build --profile --force-dirty --ghc-options=-fprof-auto-top arrow:arrow-profile
stack exec -- arrow-profile
hp2ps -e8in -c arrow-profile.hp
vim arrow-profile.prof
