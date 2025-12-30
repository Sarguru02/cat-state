{
  description = "Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/ee930f9755f58096ac6e8ca94a1887e0534e2d81";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixos-unified.url = "github:srid/nixos-unified";
    haskell-flake.url = "github:srid/haskell-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    superposition.url = "github:juspay/superposition/haskell-provider";
    superposition.inputs.nixpkgs.follows = "nixpkgs";

    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;
  };

  outputs = inputs:
    # This will import ./nix/modules/flake/*.nix
    # cf. https://nixos-unified.org/autowiring.html#flake-parts
    #
    # To write your own Nix, add or edit files in ./nix/modules/flake/
    inputs.nixos-unified.lib.mkFlake
      { inherit inputs; root = ./.; };
}
