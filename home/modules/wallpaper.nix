{ config, pkgs, lib, ... }:
let
  picturesDir   = "${config.home.homeDirectory}/Pictures";
  wallsDir      = "${picturesDir}/wallpapers";
  shotsDir      = "${picturesDir}/screenshots";
  stateDir      = "${config.xdg.stateHome or "${config.home.homeDirectory}/.local/state"}/wallpaper";
  currentLink   = "${stateDir}/current";
  swaybgPidfile = "${stateDir}/swaybg.pid";

  wallpaperApply = pkgs.writeShellScriptBin "wallpaper-apply" ''
    set -Eeuo pipefail
    WALL="''\${1:-${currentLink}}"
    test -e "$WALL" || { echo "No wallpaper found: $WALL" >&2; exit 1; }

    if [[ -f "${swaybgPidfile}" ]] && kill -0 "$(cat ${swaybgPidfile})" 2>/dev/null; then
      kill "$(cat ${swaybgPidfile})" || true
      rm -f "${swaybgPidfile}"
    fi

    setsid ${pkgs.swaybg}/bin/swaybg -m fill -i "$WALL" >/dev/null 2>&1 &
    echo $! > "${swaybgPidfile}"
  '';

  wallpaperRandom = pkgs.writeShellScriptBin "wallpaper-random" ''
    set -Eeuo pipefail
    walls="${wallsDir}"
    mapfile -t files < <(find "$walls" -type f \( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' -o -iname '*.webp' \) | sort)

    count="''\${#files[@]}"
    if (( count == 0 )); then
      echo "No images in $walls" >&2
      exit 1
    fi

    pick="''\${files[$RANDOM % ''\${#files[@]}]}"

    mkdir -p "${stateDir}"
    rm -f "${currentLink}"
    ln -s "$pick" "${currentLink}"

    exec ${wallpaperApply}/bin/wallpaper-apply "${currentLink}"
      '';

in
{
  # Ensure directories exist
  home.activation.createWallpaperDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    install -d -m 755 ${lib.escapeShellArg picturesDir} \
                       ${lib.escapeShellArg wallsDir} \
                       ${lib.escapeShellArg shotsDir} \
                       ${lib.escapeShellArg stateDir}
  '';

  home.packages = [
    wallpaperApply 
    wallpaperRandom
    pkgs.swaybg
    pkgs.findutils
    pkgs.coreutils
    pkgs.gawk
  ];

  # Service to apply current wallpaper at login
  systemd.user.services.wallpaper = {
    Unit = {
      Description = "Apply current wallpaper with swaybg";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${wallpaper-apply}/bin/wallpaper-apply ${currentLink}";
      Restart = "on-failure";
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };
}
