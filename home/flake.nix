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
    # Allow overriding from CLI if you want: --system aarch64-linux
    system = builtins.currentSystem or "aarch64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    homeConfigurations."username" = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
                stylix.homeManagerModules.stylix
        ./home.nix
        { home.packages = [ home-manager.packages.${system}.default ]; }
      ];
      # (Optional) Pass inputs to your modules if you want to reference them
      extraSpecialArgs = { inherit system; };
    };
  };
}
