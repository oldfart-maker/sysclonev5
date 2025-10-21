# modules/wallpaper.nix
{ config, lib, pkgs, ... }:
let
  picturesDir   = "${config.home.homeDirectory}/Pictures";
  wallsDir      = "${picturesDir}/wallpapers";
  shotsDir      = "${picturesDir}/screenshots";
  stateDir      = "${config.xdg.stateHome or "${config.home.homeDirectory}/.local/state"}/wallpaper";
  currentLink   = "${stateDir}/current";    # symlink to the chosen file
  swaybgPidfile = "${stateDir}/swaybg.pid";
in
{
  # 1) Ensure directories exist (idempotent)
  home.activation.createWallpaperDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    install -d -m 755 ${lib.escapeShellArg picturesDir} \
                       ${lib.escapeShellArg wallsDir} \
                       ${lib.escapeShellArg shotsDir} \
                       ${lib.escapeShellArg stateDir}
  '';

  # 2) Scripts: pick/apply wallpaper and (re)start swaybg
  home.packages = [ pkgs.coreutils pkgs.findutils pkgs.gawk pkgs.swaybg ];

  home.file."bin/wallpaper-apply".text = ''
    #!/usr/bin/env bash
    set -Eeuo pipefail
    WALL="${1:-${currentLink}}"
    test -e "$WALL" || { echo "No wallpaper found: $WALL" >&2; exit 1; }

    # Stop any running swaybg we started
    if [[ -f "${swaybgPidfile}" ]] && kill -0 "$(cat ${swaybgPidfile})" 2>/dev/null; then
      kill "$(cat ${swaybgPidfile})" || true
      rm -f "${swaybgPidfile}"
    fi

    # Spawn new swaybg for all outputs (adjust args to taste)
    # --mode fill|fit|stretch|center|tile
    setsid swaybg -m fill -i "$WALL" >/dev/null 2>&1 &
    echo $! > "${swaybgPidfile}"
  '';
  home.file."bin/wallpaper-apply".permissions = "0755";

  home.file."bin/wallpaper-random".text = ''
    #!/usr/bin/env bash
    set -Eeuo pipefail
    walls="${wallsDir}"
    mapfile -t files < <(find "$walls" -type f \( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' -o -iname '*.webp' \) | sort)
    (( ${#files[@]} )) || { echo "No images in $walls" >&2; exit 1; }
    pick="${files[$RANDOM % ${#files[@]}]}"

    mkdir -p "${stateDir}"
    rm -f "${currentLink}"
    ln -s "$pick" "${currentLink}"

    # apply immediately
    "${HOME}/bin/wallpaper-apply" "${currentLink}"

    echo "Wallpaper -> $pick"
  '';
  home.file."bin/wallpaper-random".permissions = "0755";

  # 3) Systemd user service: apply current wallpaper on login
  systemd.user.services.wallpaper = {
    Unit = {
      Description = "Apply current wallpaper with swaybg";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${config.home.homeDirectory}/bin/wallpaper-apply ${currentLink}";
      Restart = "on-failure";
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };

  # 4) OPTIONAL: auto-rotate every ~3 days (with some jitter)
  # Enable both the service above and this timer if you want automatic changes.
  systemd.user.services.wallpaper-random = {
    Unit = { Description = "Pick a random wallpaper and apply it"; };
    Service = {
      Type = "oneshot";
      ExecStart = "${config.home.homeDirectory}/bin/wallpaper-random";
    };
    Install = { WantedBy = [ "default.target" ]; };
  };

  systemd.user.timers.wallpaper-random = {
    Unit = { Description = "Rotate wallpaper every 3 days"; };
    Timer = {
      OnCalendar = "daily";           # run daily…
      RandomizedDelaySec = "3d";      # …but delay randomly up to 3 days → ~every 3 days
      Persistent = true;
    };
    Install = { WantedBy = [ "timers.target" ]; };
  };
}
