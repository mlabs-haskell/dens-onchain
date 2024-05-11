Smart contract implementation + tests for [DeNS](https://github.com/mlabs-haskell/dens). 

This repo exists because making PSM compatible with other Nix dependencies in the main repository proved to be exceedingly annoying. 

To export the scripts, enter the nix shell with `nix develop`. Then `cabal run`, and the scripts will be serialized into a standard envelope format and written to the `scripts` directory.


