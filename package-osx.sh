#!/bin/sh
set -eu

app=$(basename $PWD)

nix-shell --run "cabal clean && cabal build $app"
cp -f dist/build/$app/$app.jsexe/all.js $app/

cp -f dist/build/$app/$app.jsexe/all.js $app/
cp -fR files/* $app/
mv $app/rn-cli.config.desktop.js $app/rn-cli.config.js

dir=$PWD
nix-shell -p nodejs-5_x --run "cd $app/osx && xcodebuild -scheme $app -configuration Release -target $app archive -archivePath $dir/dist/$app.xcarchive"
hdiutil create -volname $app -srcfolder dist/$app.xcarchive/Products/Applications/$app.app -ov -format UDZO dist/$app.dmg

echo "Generated dist/$app.app"