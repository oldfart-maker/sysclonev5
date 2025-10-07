{ config, pkgs, lib, ... }:
let
  cfg = config.sysclone.emacs;

  emacsProfile = pkgs.writeShellScriptBin "emacs-profile" ''
    set -Eeuo pipefail
    export WAYLAND_DISPLAY="''${WAYLAND_DISPLAY:-wayland-0}"
    export XDG_RUNTIME_DIR="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
    exec ${pkgs.emacs}/bin/emacsclient -c -a ""
  '';

  emacsRestart = pkgs.writeShellScriptBin "emacsr" ''
    set -Eeuo pipefail
    if systemctl --user list-unit-files | grep -q '^emacs.service'; then
      systemctl --user restart emacs.service
      systemctl --user --no-pager status emacs.service --lines=5 || true
    else
      ${pkgs.emacs}/bin/emacsclient -e '(kill-emacs)' >/dev/null 2>&1 || true
      ${pkgs.emacs}/bin/emacsclient -c -a "" >/dev/null 2>&1 || true
      echo "Emacs daemon restarted via emacsclient."
    fi
  '';
in
{
  options.sysclone.emacs = {
    enable = lib.mkEnableOption "Enable Emacs + daemon + helpers";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.emacs;  # override with pkgs.emacs-wayland, pkgs.emacs-git, etc.
      description = "Emacs package to use.";
    };

    # Keep as string (not lib.types.path) so evaluation stays pure.
    commonDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.config/emacs-common";
      description = "Shared Emacs files dir (string path).";
    };

    tangle = {
      enable = lib.mkEnableOption "Run a user-provided tangle command at switch time";
      command = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Shell command to tangle (e.g., emacsclient -e ...).";
      };
    };
  };

  config = lib.mkIf (cfg.enable or true) {
    programs.emacs = {
      enable  = true;
      package = cfg.package;
    };

    services.emacs = {
      enable = true;
      client.enable = true;
    };

    home.packages = with pkgs; [
      ripgrep
      fd
      emacsProfile
      emacsRestart
    ];

    # Make sure ~/.config/emacs-common exists (string path)
    home.activation.ensureEmacsCommonDir =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        mkdir -p "${cfg.commonDir}"
      '';

    # Optional tangle step
    home.activation.tangleEmacs = lib.mkIf cfg.tangle.enable (
      lib.hm.dag.entryAfter [ "ensureEmacsCommonDir" ] ''
        if [ -n "${cfg.tangle.command}" ]; then
          echo "[hm:emacs] tangle..."
          bash -lc ${lib.escapeShellArg cfg.tangle.command}
        fi
      ''
    );
  };
}
