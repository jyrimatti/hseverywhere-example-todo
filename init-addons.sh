#! /usr/bin/env nix-shell
#! nix-shell --pure -i bash -p nix bash
set -eu
source ./nix-shell-init.sh

buildToolsVersion=$(nix-store --query --references $(nix-instantiate '<nixpkgs>' -A androidsdk_9_0) | grep 'build-tools' | sed 's/.*build-tools-\([^.]*\).*.drv/\1/')

cp register_addons*.js rnproject/

nix-shell -p nodejs-10_x --run "cd rnproject && cat ../addons.txt | grep -v '^#' | xargs -L1 npm install --save --no-audit"
nix-shell -p nodejs-10_x --run "cd rnproject && cat ../addons.txt | grep -v '^#' | sed 's/@.*//' | xargs -L1 node_modules/.bin/react-native link"

# change addon android buildTools version to that available from nixpkgs
cd rnproject && cat ../addons.txt | grep -v '^#' | sed 's/@.*//' | xargs -I {} sed -i "s/buildToolsVersion \"[^\"]*\"/buildToolsVersion \"$buildToolsVersion\"/" node_modules/{}/android/build.gradle || echo ' skipping...'
