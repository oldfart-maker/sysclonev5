# modules/wallpaper.nix
{ config, pkgs, lib, ... }:

let
  # Folders
  picturesDir = "${config.home.homeDirectory}/Pictures";
  wallsDir    = "${picturesDir}/wallpapers";

  # Per-user state (no git; survives switches)
  stateDir    = "${config.xdg.stateHome or "${config.home.homeDirectory}/.local/state"}/wallpaper";
  currentLink = "${stateDir}/current";
  pidFile     = "${stateDir}/bg.pid";

  # Detect a Wayland session (usable from keybinds/tty)
  detectWayland = ''
    export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
    if test -z "${WAYLAND_DISPLAY:-}"; then
      if test -n "$XDG_RUNTIME_DIR"; then
        wd="$(ls "$XDG_RUNTIME_DIR" 2>/dev/null | grep -E '^wayland-' | head -n1 || true)"
        test -n "$wd" && export WAYLAND_DISPLAY="$wd"
      fi
    fi
  '';

  # Apply the given image path (or ${currentLink}) with swww or swaybg
  wallpaperApply = pkgs.writeShellScriptBin "wallpaper-apply" ''
    set -Eeuo pipefail

    mkdir -p "${stateDir}"

    ${detectWayland}

    # choose target image
    img="''${1:-${currentLink}}"
    if [ ! -e "$img" ]; then
      echo "[wallpaper-apply] missing image: $img" >&2
      exit 2
    fi

    # Prefer swww if present (smooth transitions), else swaybg
    if command -v swww >/dev/null 2>&1; then
      # start daemon if not running
      swww query >/dev/null 2>&1 || swww-daemon --no-daemonize &
      # give daemon a moment on first start
      for i in 1 2 3; do swww query >/dev/null 2>&1 && break || sleep 0.15; done
      # crossfade
      swww img "$img" --transition-type any --transition-duration 0.35
    else
      # kill previous swaybg (best-effort)
      if [ -f "${pidFile}" ]; then
        if kill -0 "$(cat "${pidFile}")" 2>/dev/null; then
          kill "$(cat "${pidFile}")" 2>/dev/null || true
          # tiny grace period
          sleep 0.05
        fi
        rm -f "${pidFile}"
      fi

      # set wallpaper on all outputs (-m fill usually looks good)
      setsid -f ${pkgs.swaybg}/bin/swaybg -m fill -i "$img" 1>/dev/null 2>&1 &
      echo $! > "${pidFile}"
    fi
  '';

  # Pick a random image from ${wallsDir}, update ${currentLink}, optionally re-theme
  wallpaperRandom = pkgs.writeShellScriptBin "wallpaper-random" ''
    set -Eeuo pipefail
    mkdir -p "${stateDir}"

    dir="${wallsDir}"
    test -d "$dir" || { echo "[wallpaper-random] not a dir: $dir" >&2; exit 3; }

    # find images (jpg/png/webp), pick one
    mapfile -t imgs < <(find "$dir" -type f \( -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.png' -o -iname '*.webp' \) | sort)
    test "''${#imgs[@]}" -gt 0 || { echo "[wallpaper-random] no images in $dir" >&2; exit 4; }

    pick="''${imgs[RANDOM % ''${#imgs[@]}]}"

    # refresh the 'current' symlink atomically
    tmpLink="$(mktemp -u "${currentLink}.XXXX")"
    ln -sfT "$pick" "$tmpLink"
    mv -f "$tmpLink" "${currentLink}"

    echo "[wallpaper-random] -> $pick"

    # apply to the running session
    wallpaper-apply "${currentLink}"

    # Optional: if the first arg is --retheme, ask Home-Manager to re-generate Stylix colors from the current image
    if [ "''${1:-}" = "--retheme" ]; then
      if command -v home-manager >/dev/null 2>&1; then
        echo "[wallpaper-random] re-theming via Home Manager..."
        home-manager switch >/dev/null
      else
        echo "[wallpaper-random] home-manager not found, skipping retheme." >&2
      fi
    fi
  '';
in
{
  # Provide the tools
  home.packages = [
    wallpaperApply
    wallpaperRandom
    pkgs.swaybg
    pkgs.findutils
    pkgs.coreutils
    pkgs.gawk
    pkgs.swww     # optional; remove if you don't want it
  ];

  # (Optional) a tiny user service to re-apply the last wallpaper on login
  # systemd.user.services."wallpaper-apply" = {
  #  Unit = {
  #    Description = "Apply last wallpaper";
  #    After = [ "graphical-session-pre.target" ];
  #    PartOf = [ "graphical-session.target" ];
  #  };
  #  Service = {
  #    Type = "oneshot";
  #    ExecStart = "${wallpaperApply}/bin/wallpaper-apply";
  #    Environment = [ "XDG_RUNTIME_DIR=%t" ];
  #  };
  #  Install = { WantedBy = [ "graphical-session.target" ]; };
  # };
}
