# home/flake.nix
{
  description = "sysclonev5 HM (Pi)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }:
  let
    # NEW: overlay which forces Alacritty to build with Wayland + X11
    overlays = [
      (final: prev: {
        alacritty = prev.alacritty.override {
          waylandSupport = true;
          x11Support = true;
        };
      })
    ];

    forSystem = system:
      import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = overlays;        # NEW
      };
  in {
    homeConfigurations.username = home-manager.lib.homeManagerConfiguration {
      pkgs = forSystem "aarch64-linux";
      modules = [ ./home.nix ];
    };
  };
}
