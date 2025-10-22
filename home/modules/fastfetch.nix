{ config, pkgs, lib, ... }:
let
  repoRoot = "${config.home.homeDirectory}/projects/sysclonev5";
  cfg = "${repoRoot}/dotfiles/fastfetch/config.jsonc";
in {
  home.packages = [ pkgs.fastfetch ];

  # Bake contents (recommended):
  xdg.configFile."fastfetch/config.jsonc".text = builtins.readFile cfg;

  # Or symlink:
  # xdg.configFile."fastfetch/config.jsonc".source = lib.file.mkOutOfStoreSymlink cfg;
}
