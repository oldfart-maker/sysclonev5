{ config, pkgs, lib, ... }:
let
  emacsPkg = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk);
in {
  # Put emacs/emacsclient on PATH
  home.packages = [ emacsPkg ];

  # Disable the generic HM service (we’ll run our own named daemon)
  services.emacs.enable = lib.mkForce false;

  # Named daemon: emacs-prod
  systemd.user.services."emacs-prod" = {
    Unit = {
      Description = "Emacs daemon (emacs-prod)";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${emacsPkg}/bin/emacs --fg --daemon=emacs-prod";
      Restart = "on-failure";
      # Light Wayland-friendly env; harmless on X11 too
      Environment = [
        "WAYLAND_DISPLAY=${lib.mkDefault "wayland-0"}"
        "XDG_RUNTIME_DIR=/run/user/%U"
      ];
    };
    Install = { WantedBy = [ "default.target" ]; };
  };

  # Ensure ~/.local/bin is on PATH and add a helper that targets the right socket
  home.sessionPath = [ "$HOME/.local/bin" ];

  home.file.".local/bin/emacsp" = {
    text = ''
      #!/usr/bin/env bash
      exec ${emacsPkg}/bin/emacsclient -c -n --socket-name=emacs-prod "$@"
    '';
    executable = true;
  };
}

