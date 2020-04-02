#!/bin/sh

nix-shell --run "JEKYLL_ENV=production jekyll build"
