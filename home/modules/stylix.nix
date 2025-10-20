# modules/stylix.nix
{ config, pkgs, lib, ... }:

{
  # Pick ONE: image-based palette OR a fixed Base16 scheme.
  stylix = {
    enable   = true;

    # Option A: derive palette from an image
    image    = "${config.home.homeDirectory}/Pictures/wallpapers/waves.jpg";
    polarity = "dark";

    # Option B: fixed Base16 scheme (uncomment & remove `image` above)
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";

    targets = {
      waybar = {
        enable = true;
        addCss = false;  # we’ll provide our own style/colors
      };
      rofi.enable = true;
    };
  }
}
