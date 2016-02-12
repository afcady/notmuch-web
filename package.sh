#!/bin/bash

args=
if [ "$1" != "--icu" ]; then
	args="-fno-icu"
fi

f="notmuch-web-`grep ^version notmuch-web.cabal | awk '{print $2}'`"
r="releases/$f"

cabal clean
cabal configure $args
cabal build
strip dist/build/notmuch-web/notmuch-web
mkdir -p $r
cp dist/build/notmuch-web/notmuch-web $r
cp README.md $r
mkdir -p $r/config
cp config/favicon.ico $r/config
cp config/robots.txt $r/config
cp config/settings.yml $r/config/settings.example.yml
cd releases
tar cvzf "$f.`uname -m`.tar.gz" $f
