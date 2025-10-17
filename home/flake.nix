{
  description = "sysclonev5 HM (Pi)";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };


  outputs = { self, nixpkgs, home-manager, ... }:
  let
    forSystem = system: import nixpkgs { inherit system; config.allowUnfree = true; };
  in {
    homeConfigurations.username = home-manager.lib.homeManagerConfiguration {
      pkgs = forSystem "aarch64-linux";
      modules = [
        ./home.nix
        ./modules/emacs.nix
      ];
    };
  };
}
