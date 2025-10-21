# modules/wallpaper.nix
{ config, pkgs, lib, ... }:
let
  picturesDir   = "${config.home.homeDirectory}/Pictures";
  wallsDir      = "${picturesDir}/wallpapers";
  shotsDir      = "${picturesDir}/screenshots";
  stateDir      = "${config.xdg.stateHome or "${config.home.homeDirectory}/.local/state"}/wallpaper";
  currentLink   = "${stateDir}/current";
  swaybgPidfile = "${stateDir}/swaybg.pid";

  # Apply the selected wallpaper (from arg or ${currentLink})
  wallpaperApply = pkgs.writeShellScriptBin "wallpaper-apply" ''
    set -Eeuo pipefail

    # If WAYLAND_DISPLAY isn't set (e.g., ssh), try to detect it from sockets.
    if [[ -z "''${WAYLAND_DISPLAY:-}" ]]; then
      uid="$(id -u)"
      sock="$(ls -1 /run/user/"$uid"/wayland-* 2>/dev/null | head -n1 || true)"
      if [[ -S "$sock" ]]; then
        export WAYLAND_DISPLAY="$(basename "$sock")"
      fi
    fi

    # If still not in Wayland, just succeed quietly.
    if [[ -z "''${WAYLAND_DISPLAY:-}" && -z "''${SWAYSOCK:-}" ]]; then
      exit 0
    fi

    WALL="''\${1:-${currentLink}}"
    # If no current, try to pick one once.
    if [[ ! -e "$WALL" ]]; then
      if compgen -G "${wallsDir}/*.{jpg,jpeg,png,webp}" > /dev/null; then
        pick="$(find "${wallsDir}" -type f \( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' -o -iname '*.webp' \) | sort | shuf -n1)"
        mkdir -p "${stateDir}"
        rm -f "${currentLink}"
        ln -s "$pick" "${currentLink}"
        WALL="${currentLink}"
      else
        exit 0
      fi
    fi

    # Stop any previous swaybg we started
    if [[ -f "${swaybgPidfile}" ]] && kill -0 "$(cat ${swaybgPidfile})" 2>/dev/null; then
      kill "$(cat ${swaybgPidfile})" || true
      rm -f "${swaybgPidfile}"
    fi

    # Launch swaybg
    setsid ${pkgs.swaybg}/bin/swaybg -m fill -i "$WALL" >/dev/null 2>&1 &
    echo $! > "${swaybgPidfile}"
  '';

  # Pick a random wallpaper, store it as ${currentLink}, and (if Wayland) apply it
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

    # Apply only if Wayland is present; otherwise exit 0
    if [[ -n "''${WAYLAND_DISPLAY:-}" || -n "''${SWAYSOCK:-}" ]]; then
      exec ${wallpaperApply}/bin/wallpaper-apply "${currentLink}"
    else
      exit 0
    fi
  '';
in
{
  # Ensure directories exist (idempotent)
  home.activation.createWallpaperDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    install -d -m 755 ${lib.escapeShellArg picturesDir} \
                       ${lib.escapeShellArg wallsDir} \
                       ${lib.escapeShellArg shotsDir} \
                       ${lib.escapeShellArg stateDir}
  '';

 # Wallpaper is managed outside of the context of HM, so we need to re-apply
 # the wall paper after a switch.
home.activation.reapplyWallpaper = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  ${pkgs.bash}/bin/bash -lc "wallpaper-apply" >/dev/null 2>&1 || true
'';

  # Install the scripts + tools
  home.packages = [
    wallpaperApply
    wallpaperRandom
    pkgs.swaybg
    pkgs.findutils
    pkgs.coreutils
    pkgs.gawk
  ];

  # (Intentionally no systemd.user.services here)
}
