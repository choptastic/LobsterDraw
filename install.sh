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
ln -s $ROOT/site working/nitrogen/rel/nitrogen/site
ln -s $ROOT/etc  working/nitrogen/rel/nitrogen/etc
mv working/nitrogen/rel/nitrogen $ROOT/game
rm -fr working
cd game
make clean
make cookie
make copy-static
make
echo "Game built in game/"
echo "To run game, cd into game and run 'bin/nitrogen console'"
