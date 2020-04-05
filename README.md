# ACStalks.com

The Haskell package in the root directory provides the backend service + the static file server for ACStalks.com (written with the Servant framework).

`./frontend` contains the Jekyll-powered frontend for ACStalks.com. 

This package is set up to build and run on a Nix-enabled system; please ensure that `stack` is able to build with `--nix` and that you are able to use `nix-shell`.

To generate a new database (or to migrate an old schema version to the latest, run `./utils/newdb.sh` in the root of this repository.

To set up Jekyll, run `./refresh_dependencies.sh` in the root of `./frontend/refresh_dependencies.sh`.
To generate a new version of the frontend from the sources, run `./frontend/build.sh` in the root of `./frontend`

To start the backend + the static file server that will serve the generated Jekyll frontend (from `./frontend/_site`), run `stack run` in the root of this repository. The server will serve from port 8080 by default; it is recommended to run this site under a TLS-enabled reverse proxy in production.

