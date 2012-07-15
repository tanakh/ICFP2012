#!/bin/bash

MATCHDIR=$(pwd)
PROJECT_ROOT=$(dirname $MATCHDIR)

echo $MATCHDIR
echo $PROJECT_ROOT

function build_run() {
    git pull | grep 'Already up-to-date'
    if [ $! -ne 0 ]; then
        (cd ../simulator/ && cabal configure && cabal build)
        (cabal configure && cabal build)
        cp dist/build/run_ais/run_ais .
    fi

    if [ ! -d results ]; then
        mkdir results
    fi

    results_dir=results.`date +%s`
    rsync -av results/ $results_dir
    cd $results_dir && ../genlist.sh ../.. && ../run_ais -a ailist -m maps -l 150
}

while true; do
    build_run
    sleep 60
done
