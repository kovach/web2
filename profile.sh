set -e
stack build --profile
stack exec -- arrow-profile
#hp2ps -e8in -c main.hp
vim arrow-profile.prof
