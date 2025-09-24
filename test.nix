{ pkgs ? import <nixpkgs> { }, ... }:

let 
  pgData   = "./pgData";
  testPkgs = ghc: [ 
    ghc.HUnit 
    ghc.regex-compat 
    ghc.HDBC
    ghc.HDBC-postgresql
  ];
in

pkgs.mkShell {
  inputsFrom  = [
    (pkgs.haskell.packages.ghc90.callCabal2nix "haskelldb-2.2.4.1" ./. { }).env 
    (pkgs.haskell.packages.ghc90.ghcWithPackages testPkgs)
  ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.postgresql
  ];
  shellHook = ''
    cabal build

    echo "Setting up PostgreSQL..."
    export PGDATA="$(realpath ${pgData})"
    echo "Initializing database cluster in local directory at $PGDATA, will run db server with current user $(whoami)"
    if [ -d "$PGDATA" ]; then
      rm -rf "$PGDATA"
    fi
    mkdir -p "$PGDATA"

    initdb --auth-local=trust --auth-host=trust --pgdata "$PGDATA" -c "unix_socket_directories=$PGDATA" 
    pg_ctl --pgdata="$PGDATA" -l "$PGDATA/logfile" start 

    trap "echo 'Stopping PostgreSQL'; pg_ctl --pgdata=$PGDATA -w stop" EXIT
  '';
}
