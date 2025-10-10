{
  imports = [
    ./modules/emacs.nix
    ./modules/emacs-babel.nix
    ./modules/fonts.nix
  ];

  home.username = "username";
  home.homeDirectory = "/home/username";
  home.stateVersion = "24.05";
  programs.home-manager.enable = true;
}
