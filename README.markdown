# LobsterDraw

A pictionary/DrawSomething type game written in Erlang for the Nitrogen Web Framework for the Spawnfest 2011 Competition

## Installation



Copyright (c) 2011-2013 by Jesse Gumm
MIT-License
@jessegumm
http://sigma-star.com/page/jesse


A Pictionary-inspired game written for the Erlang-based Nitrogen Web Framework for the Spawnfest 2011 Competition.

This is using the latest (as of 2011-07-10) version of the Nitrogen Web Framework found on Github (http://github.com/nitrogen)

To run, you'll want to first run 'make' then 'bin/nitrogen console'

It includes a copy of erts 5.8.4 and uses mochiweb as it's webserver.

The only really interesting part (where the code goes) is in site/

The web page code are found in site/pages

Anmy behind-the-scenes code (gen_servers and processing stuff) are found in site/lib

There is a word list found at ./site/words.txt

Feel free to change that word list.  Right now, it's just a handful of words I found on a quick google search for "pictionary words"

THIS WAS NOT TESTED IN ANY VERSION OF INTERNET EXPLORER
