{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "web";
  buildInputs = [ postgresql lzma.dev git ];
}
