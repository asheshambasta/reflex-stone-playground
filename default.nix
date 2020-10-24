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
  packages = { reflex-stone = ./frontend; };
  shells = {
    ghc = [ "reflex-stone" ];
    ghcjs = [ "reflex-stone" ];
  };

  overrides = self: super: { 
    bulmex = self.callCabal2nix "bulmex" ../bulmex/bulmex {};
    reflex-dom-helpers = self.callCabal2nix "reflex-dom-helpers" ../reflex-dom-helpers {};
  };

})
