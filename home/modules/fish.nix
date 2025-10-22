{ config, pkgs, lib, ... }:
let
  repoRoot = "${config.home.homeDirectory}/projects/sysclonev5";
  dot      = name: "${repoRoot}/dotfiles/${name}";
in {
  programs.fish.enable = true;
  home.packages = [ pkgs.fish ];

  # Bake contents (rollback-safe):
  home.file.".config/fish/config.fish".text = builtins.readFile (dot "fish/config.fish");
  home.file.".config/fish/conf.d".source =
    lib.file.mkOutOfStoreSymlink (dot "fish/conf.d"); # directory: symlink is fine

  # If you prefer all symlinks instead:
  # home.file.".config/fish/config.fish".source = lib.file.mkOutOfStoreSymlink (dot "fish/config.fish");
}
