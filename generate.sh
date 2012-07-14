#!/bin/bash -ev

ID=96526636
ARC="icfp-${ID}"

rm -rf ${ARC}

cd simulator
cabal-dev clean
cabal-dev install
cabal-dev sdist
cd ../

cp -r template ${ARC}
cp "simulator/cabal-dev/bin/$1" ${ARC}/lifter

mkdir ${ARC}/src
cp -r simulator/dist/*.tar.gz ${ARC}/src

cd ${ARC}
tar -cvzf "../${ARC}.tgz" install lifter PACKAGES src README
cd ..
rm -rf ${ARC}
