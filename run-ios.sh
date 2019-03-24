#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix bash
set -eu
source ./nix-shell-init.sh

### Note! Note pure, requires xcode! ###

port=${1:-8081}

nix-shell --pure --run "sed -i \"s/8081/$port/g\" rnproject/ios/rnproject/AppDelegate.m"

nix-shell -p nodejs-10_x --run "(cd rnproject && ./node_modules/.bin/react-native run-ios --no-packager)"
