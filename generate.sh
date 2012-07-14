ID = 96526636
ARC = "icfp-${ID}"

cd simulator
cabal update
cabal-dev clean
cabal-dev install
cd ../

cp -r template ${ARC}
cp simulator/cabal-dev/bin/$1 lifter

cd simulator
cabal-dev clean
cd ..
cp -r simulator ${ARC}/src

cd ${ARC}
tar -xf "../${ARC}.tgz" install lifter PACKAGES src README
cd ..
rm -rf $ARC
