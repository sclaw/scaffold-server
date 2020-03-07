{ghc}:
with (import <nixpkgs> {});


haskell.lib.buildStackProject {
  inherit ghc;
  name = "server";
  buildInputs = [postgresql lzma.dev git zlib perl];
}
