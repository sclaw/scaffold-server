{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "edgeNode-server";
  buildInputs = [ postgresql lzma.dev git zlib ];
}
