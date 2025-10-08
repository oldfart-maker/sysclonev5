{ config, pkgs, lib, ... }:

{
  # 1) Install just the Emacs binary (Wayland build on ARM)
  home.packages = [ pkgs.emacs-pgtk ];

  # 2) A shared spot for cross-profile assets; we’ll use it later
  xdg.configFile."emacs-common/.keep".text = "";

  # 3) Run Emacs as a user systemd daemon; socket-activated
  services.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    # Optional, handy: let `editor`/`git commit` use emacsclient
    defaultEditor = true;
    startWithUserSession = true;
  };
}
