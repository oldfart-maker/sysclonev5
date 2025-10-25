{ config, ... }:

{
  # Create the standard XDG dirs (includes ~/Pictures)
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };

  # Ensure the subfolders always exist
  systemd.user.tmpfiles.rules = [
    "d %h/Pictures 0755 - - - -"
    "d %h/Pictures/screenshots 0755 - - - -"
    "d %h/Pictures/wallpapers 0755 - - - -"
  ];
}
