{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.tcp-port.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ (with pkgs; [ 
    hlint
    stylish-haskell
    haskell-language-server
  ]);
})