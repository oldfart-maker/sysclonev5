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

    # If we're not in a Wayland/Niri session (e.g., SSH), do nothing but succeed.
    if [[ -z "''${WAYLAND_DISPLAY:-}" && -z "''${SWAYSOCK:-}" ]]; then
      exit 0
    fi

    WALL="''\${1:-${currentLink}}"

    # If no current selection, try to pick one from the wallpapers dir
    if [[ ! -e "$WALL" ]]; then
      if compgen -G "${wallsDir}
       /*.{jpg,jpeg,png,webp}" > /dev/null; then
        pick="$(find "${wallsDir}" -type f \( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' -o -iname '*.webp' \) | sort | shuf -n1)"
        mkdir -p "${stateDir}"
        rm -f "${currentLink}"
       n ln -s "$pick" "${currentLink}"
        WALL="${currentLink}"
      else
        # No images; succeed quietly (nothing to draw)
        exit 0
      fi
    fi

    # Stop any swaybg we previously started
    if [[ -f "${swaybgPidfile}" ]] && kill -0 "$(cat ${swaybgPidfile})" 2>/dev/null; then
      kill "$(cat ${swaybgPidfile})" || true
      rm -f "${swaybgPidfile}"
    fi

    # Start swaybg
    setsid ${pkgs.swaybg}/bin/swaybg -m fill -i "$WALL" >/dev/null 2>&1 &
    echo $! > "${swaybgPidfile}"
  '';

  wallpaperRandom = pkgs.writeShellScriptBin "wallpaper-random" ''
    set -Eeuo pipefail

    # If not in Wayland, still select/record a new wallpaper so it persists for next GUI login.
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

}
