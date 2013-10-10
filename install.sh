#!/bin/sh

ROOT=`pwd`
mkdir working
cd working
git clone git://github.com/nitrogen/nitrogen
cd nitrogen
make rel_mochiweb
cd rel/nitrogen
rm -fr site
rm -fr etc
cd $ROOT
cp -Rv site working/nitrogen/rel/nitrogen/site
cp -Rv etc  working/nitrogen/rel/nitrogen/etc
mv working/nitrogen/rel/nitrogen ./game
rm -fr working/nitrogen
cd game
make clean
make
echo "Game built in game/"
echo "To run game, cd into game and run 'bin/nitrogen console'"
