# Configuration for the project's Nix devShell
# You mostly want the `packages` option below.

{
  perSystem = { config, inputs', pkgs, ... }: {
    # Default shell.
    devShells.default = pkgs.mkShell {
      name = "cat-state";
      meta.description = "Haskell development environment";

      # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell # See ./nix/modules/haskell.nix
        config.pre-commit.devShell # See ./nix/modules/formatter.nix
        inputs'.superposition.devShells.haskell
      ];

      # Packages to be added to Nix devShell go here.
      packages = with pkgs; [
        just
        nixd
        ghciwatch
      ];
    };
  };
}
