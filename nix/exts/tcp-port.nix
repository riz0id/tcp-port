{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        tcp-port = self.callCabal2nix "tcp-port" ../../. { };
      });
    };
  };
}