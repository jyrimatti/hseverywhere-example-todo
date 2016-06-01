#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p nix bash
set -eu
export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/$(cat nix-revision.txt).tar.gz

port=${1:-8081}

androidDevice="7in WSVGA (Tablet)"

app=$(basename $PWD)

cp -f dist/build/$app/$app.jsexe/all.js $app/
cp -fR files/* $app/
mv $app/rn-cli.config.others.js $app/rn-cli.config.js

nix-shell -p androidsdk jre8 which file --run "\
     android create avd -f -n $app -b default/x86_64 -d \"$androidDevice\" -t \$(android list targets -c | tail -n 1)\
  && ANDROID_SDK_ROOT=\$(which android | xargs dirname | xargs dirname)/libexec/android-sdk-macosx emulator -avd $app -no-skin -no-boot-anim" &

sleep 15
read -t 120  -p "Press enter when emulator is running..." yn

nix-shell -p androidsdk jre8 which nodejs-5_x --run "cd $app; ADB_INSTALL_TIMEOUT=8 ANDROID_HOME=\$(which android | xargs dirname | xargs dirname)/libexec/android-sdk-macosx node ./node_modules/react-native-cli/index.js run-android"

nix-shell -p nodejs-5_x --run "cd $app && PORT=$port npm run start"
