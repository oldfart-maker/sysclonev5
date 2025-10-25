# home/flake.nix
{
  description = "sysclonev5 HM (Pi)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, stylix, ... }:
  let
    system = builtins.currentSystem or "aarch64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    homeConfigurations."username" =
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          stylix.homeModules.stylix
          ./home.nix
        ];
        extraSpecialArgs = { inherit system; };
      };
  };
}
