{ ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.har = pkgs.mkShell {
      inherit (self'.devShells.haskell) FONTCONFIG_FILE;

      inputsFrom = [
        self'.devShells.haskell
        self'.packages.har.env
      ];
    };

    packages.har =
      pkgs.haskellPackages.callCabal2nix
        "har"
        (pkgs.nix-gitignore.gitignoreSource [ ] ./.)
        { };
  };
}
