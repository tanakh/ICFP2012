#!/bin/bash

MATCHDIR=$(readlink -f $(dirname $0))
PROJECT_ROOT=$(readlink -f "$MATCHDIR/..")

echo $MATCHDIR
echo $PROJECT_ROOT

function build() {
    (cd $PROJECT_ROOT/simulator/ && cabal configure && cabal build && cabal install)
    (cd $PROJECT_ROOT/matchlist/ && cabal configure && cabal build && cp dist/build/run_ais/run_ais . )
}

function prepare_dir() {
    cd $MATCHDIR
    if [ ! -d results ]; then
        mkdir results
        cd results
    fi
    [ -e $MATCHDIR/results/data ] || ln -s $PROJECT_ROOT/data $MATCHDIR/results/data
    [ -e $MATCHDIR/results/smallmap ] || ln -s $PROJECT_ROOT/smallmap $MATCHDIR/results/smallmap
    cd $MATCHDIR/results
    cp $PROJECT_ROOT/simulator/learned.txt .
    find smallmap/ -name "*.map" > smallmap.maplist
    find data/ -name "*.map" > data.maplist
    find $PROJECT_ROOT/ -name "ll-ai-*" -type f -perm +111 > ailist
    ruby $MATCHDIR/rename_prog.rb ailist
    cd $MATCHDIR
}

function build_run() {
    cd $MATCHDIR
    git pull
    git show-ref -s `git symbolic-ref HEAD`
    build
    cd results
    ../run_ais -a ailist -m smallmap.maplist -w 20 -m 10
    ../run_ais -a ailist -m data.maplist -w 50 -m 10
    cd $MATCHDIR
}

function run() {
    maptype="$1"
    mapdir="$PROJECT_ROOT/$maptype"
    find mapdir -name "*.map" -exec ln -s {} \;
}

while true; do
    echo "#### START: $(date) $(date +%s) ####"
    prepare_dir
    build_run
    echo "#### DONE: $(date) $(date +%s) ####"
    sleep 60
done
