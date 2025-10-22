{ config, pkgs, lib, ... }:
let
  repoRoot = "${config.home.homeDirectory}/projects/sysclonev5";
  dot      = name: "${repoRoot}/dotfiles/${name}";
in {
  programs.fish.enable = true;
  home.packages = [ pkgs.fish ];

  home.file.".config/fish/config.fish".source =
    config.lib.file.mkOutOfStoreSymlink (df "fish/config.fish");

  home.file.".config/fish/functions" = lib.mkIf hasFuncs {
    source = config.lib.file.mkOutOfStoreSymlink (df "fish/functions");
};

}
