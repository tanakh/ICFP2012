#!/bin/bash

MATCHDIR=$(realpath $(dirname $0))
PROJECT_ROOT=$(dirname $MATCHDIR)

echo $MATCHDIR
echo $PROJECT_ROOT

function build() {
    (cd $PROJECT_ROOT/simulator/ && cabal configure && cabal build)
    (cd $PROJECT_ROOT/matchlist/ && cabal configure && cabal build && cp dist/build/run_ais/run_ais . )
}

function build_run() {
    cd $MATCHDIR
    git pull | grep 'Already up-to-date'
    ret=$?
    if [ -d dist ]; then
        if [ $ret -ne 0 ]; then
            build
        fi
    else
        build
    fi
    if [ ! -d results ]; then
        mkdir results
    fi

    backup=results-`date +%s`.bak
    cd results && ../genlist.sh ../.. && ../run_ais -a ailist -m maps -l 150
    cd $MATCHDIR
}

while true; do
    build_run
    sleep 60
done
