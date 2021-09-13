{
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hs = pkgs.haskellPackages;
      pkg = hs.callCabal2nix "greeter" ./. { };
    in rec {
      defaultPackage.x86_64-linux = pkg;
      devShell.x86_64-linux = pkg.env.overrideAttrs (super: {
        nativeBuildInputs = super.nativeBuildInputs
          ++ [ hs.cabal-install hs.tasty-discover ];
      });
    };
}
