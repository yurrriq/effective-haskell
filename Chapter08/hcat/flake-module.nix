{ ... }:

{
  perSystem = { pkgs, self', ... }: {
    devShells.hcat = pkgs.mkShell {
      inherit (self'.devShells.haskell) FONTCONFIG_FILE;

      inputsFrom = [
        self'.devShells.haskell
        self'.packages.hcat.env
      ];
    };

    packages.hcat =
      pkgs.haskellPackages.callCabal2nix
        "hcat"
        (pkgs.nix-gitignore.gitignoreSource [ ] ./.)
        { };
  };
}
