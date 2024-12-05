let
  reflex-platform-src = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/develop.tar.gz";
  };
in
import "${reflex-platform-src}/shell.nix" {}