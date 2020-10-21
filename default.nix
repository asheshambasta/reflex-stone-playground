{ system ? builtins.currentSystem }:
let
  sources = import ./nix/sources.nix;
  # bulmex = import sources.bulmex;
  bulmexOverride = selfh: superh: {
    bulmex-cust = selfh.callHackage "bulmex" "4.0.0" { };
  };
  rp = import sources.reflex-platform { inherit system; };
in rp.project ({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  packages = { reflex-stone = ./.; };
  shells = {
    ghc = [ "reflex-stone" ];
    ghcjs = [ "reflex-stone" ];
  };
  overrides = self: super: {
    # bulmex = self.callHackage "bulmex" "4.0.0" {};
    bulmex = self.callCabal2nix "bulmex" ../bulmex/bulmex { };
    reflex-dom-helpers =
    self.callPackage ../bulmex/packages/reflex-dom-helpers.nix { };
  };
})
