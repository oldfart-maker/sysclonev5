{ config, pkgs, lib, ... }:
let
  emacsPkg = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk);
in {
  # Make emacs/emacsclient available on PATH
  home.packages = [ emacsPkg ];

  # User systemd service: emacs --fg-daemon=emacs-prod
  systemd.user.services."emacs-prod" = {
    Unit = {
      Description = "Emacs daemon (emacs-prod)";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${emacsPkg}/bin/emacs --fg-daemon=emacs-prod";
      Restart = "on-failure";
    };
    Install = { WantedBy = [ "default.target" ]; };
  };

  # nice-to-have
  programs.git.enable = true;
}
