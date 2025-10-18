{
  imports = [
    ./modules/emacs.nix
    ./modules/fonts.nix
    ./modules/sway.nix
    ./modules/niri.nix
  ];

  home.username = "username";
  home.homeDirectory = "/home/username";
  home.stateVersion = "24.05";
  programs.home-manager.enable = true;
}
