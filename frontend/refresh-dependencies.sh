#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bundler bundix

rm Gemfile.lock
bundler lock
bundler package --path vendor/cache --no-install
bundix

