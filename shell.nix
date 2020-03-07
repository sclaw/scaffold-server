with (import <nixpkgs> {});
let ghc = haskell.compiler.ghc882;
in haskell.lib.buildStackProject {
  inherit ghc;
  name = "server";
  buildInputs = [ postgresql lzma.dev git zlib perl];
}
