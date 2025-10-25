# modules/wallpaper.nix
{ config, pkgs, lib, ... }:

let
  # Folders (you said dir creation is handled elsewhere)
  picturesDir = "${config.home.homeDirectory}/Pictures";
  wallsDir    = "${picturesDir}/wallpapers";

  # Per-user state (kept outside git; survives HM switches)
  stateDir    = "${config.xdg.stateHome or "${config.home.homeDirectory}/.local/state"}/wallpaper";
  currentLink = "${stateDir}/current";
  pidFile     = "${stateDir}/bg.pid";

  # Detect a Wayland session (works from keybinds/TTY)
  detectWayland = ''
    # Keep XDG_RUNTIME_DIR literal for the shell; escape Nix antiquotation with ''${…}
    export XDG_RUNTIME_DIR="''${XDG_RUNTIME_DIR:-/run/user/$(${pkgs.coreutils}/bin/id -u)}"
    if test -z "''${WAYLAND_DISPLAY:-}"; then
      if test -n "$XDG_RUNTIME_DIR"; then
        wd="$(${pkgs.coreutils}/bin/ls "$XDG_RUNTIME_DIR" 2>/dev/null | ${pkgs.gnugrep}/bin/grep -E '^wayland-' | ${pkgs.coreutils}/bin/head -n1 || true)"
        test -n "$wd" && export WAYLAND_DISPLAY="$wd"
      fi
    fi
  '';

  # Apply the given image path (or ${currentLink}) with swaybg
  wallpaperApply = pkgs.writeShellScriptBin "wallpaper-apply" ''
    set -Eeuo pipefail

    mkdir -p "${stateDir}"

    ${detectWayland}
    if test -z "''${WAYLAND_DISPLAY:-}"; then
      echo "[wallpaper-apply] No Wayland session detected." >&2
      exit 10
    fi

    img="''${1:-${currentLink}}"
    if [ ! -e "$img" ]; then
      echo "[wallpaper-apply] missing image: $img" >&2
      exit 2
    fi

    # Kill previous swaybg (best effort)
    if [ -f "${pidFile}" ]; then
      if kill -0 "$(<"${pidFile}")" 2>/dev/null; then
        kill "$(<"${pidFile}")" 2>/dev/null || true
        # tiny grace period
        sleep 0.05
      fi
      rm -f "${pidFile}"
    fi
    ${pkgs.procps}/bin/pkill -x swaybg 2>/dev/null || true

    # Spawn fresh swaybg on all outputs
    setsid -f ${pkgs.swaybg}/bin/swaybg -i "$img" -m fill 1>/dev/null 2>&1 &
    echo $! > "${pidFile}"

    echo "[wallpaper-apply] set: $img"
  '';

  # Pick a random image from ${wallsDir}, update ${currentLink}, then apply it
  wallpaperRandom = pkgs.writeShellScriptBin "wallpaper-random" ''
    set -Eeuo pipefail
    mkdir -p "${stateDir}"

    dir="${wallsDir}"
    if [ ! -d "$dir" ]; then
      echo "[wallpaper-random] not a dir: $dir" >&2
      exit 3
    fi

    # find images (jpg/png/webp), pick one
    mapfile -t imgs < <(${pkgs.findutils}/bin/find "$dir" -type f \( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' -o -iname '*.webp' \) | ${pkgs.coreutils}/bin/sort)
    if [ "''${#imgs[@]}" -eq 0 ]; then
      echo "[wallpaper-random] no images in $dir" >&2
      exit 4
    fi

    pick="''${imgs[RANDOM % ''${#imgs[@]}]}"

    # refresh the 'current' symlink atomically
    tmpLink="$(${pkgs.coreutils}/bin/mktemp -u "${currentLink}.XXXX")"
    ${pkgs.coreutils}/bin/ln -sfT "$pick" "$tmpLink"
    ${pkgs.coreutils}/bin/mv -f "$tmpLink" "${currentLink}"

    echo "[wallpaper-random] -> $pick"

    # apply to the running session
    ${wallpaperApply}/bin/wallpaper-apply "${currentLink}"

    # Optional retheme trigger disabled (swww/Stylix not called here)
  '';
in
{
  home.packages = [
    wallpaperApply
    wallpaperRandom
    pkgs.swaybg
    pkgs.findutils
    pkgs.coreutils
    pkgs.procps
    pkgs.gnugrep
  ];
}
