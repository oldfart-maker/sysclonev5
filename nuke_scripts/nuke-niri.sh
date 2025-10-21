#!/usr/bin/env bash
set -euo pipefail

DO_SYSTEM=1
for arg in "$@"; do
  case "$arg" in
    --system) DO_SYSTEM=1 ;;
    *) echo "Unknown arg: $arg" >&2; exit 1 ;;
  esac
done

say() { printf '%s\n' "$*"; }

# 1) Stop/disable/unmask user service, clean unit cache
say "Stopping/disabling user niri.service…"
systemctl --user stop niri.service 2>/dev/null || true
systemctl --user disable niri.service 2>/dev/null || true
systemctl --user reset-failed niri.service 2>/dev/null || true
systemctl --user unmask niri.service 2>/dev/null || true

# 2) Remove user units + wants symlinks
say "Removing user systemd units…"
rm -f "$HOME/.config/systemd/user/niri.service"
rm -f "$HOME/.config/systemd/user/niri-shutdown.target"
rm -f "$HOME/.config/systemd/user/default.target.wants/niri.service"
rm -f "$HOME/.config/systemd/user/graphical-session.target.wants/niri.service"
systemctl --user daemon-reload || true

# 3) Remove user session entries
say "Removing user .desktop/session entries…"
rm -f "$HOME/.local/share/wayland-sessions/niri.desktop"
rm -f "$HOME/.local/share/applications/niri.desktop"
rm -f "$HOME/.config/autostart/niri.desktop"

# 4) Remove Niri config/cache/state
say "Removing Niri config/cache/state…"
rm -rf "$HOME/.config/niri"
rm -rf "$HOME/.cache/niri"
rm -rf "$HOME/.local/state/niri"

# 5) Remove any ad-hoc user/local copies of niri binaries
say "Removing ad-hoc user/local niri binaries (if any)…"
rm -f "$HOME/.local/bin/niri" "$HOME/.local/bin/niri-session"
rm -f "$HOME/.cargo/bin/niri" "$HOME/.cargo/bin/niri-session"
rm -f "$HOME/bin/niri"        "$HOME/bin/niri-session"
# Only touch /usr/local if it exists and you have --system
if [[ $DO_SYSTEM -eq 1 ]]; then
  sudo rm -f /usr/local/bin/niri /usr/local/bin/niri-session || true
fi

# 6) Optionally remove system session .desktop files
if [[ $DO_SYSTEM -eq 1 ]]; then
  say "Removing system wayland-session entries…"
  sudo rm -f /usr/share/wayland-sessions/niri.desktop || true
  sudo rm -f /usr/local/share/wayland-sessions/niri.desktop || true
fi

# 7) Final hints (purely informative)
say ""
say "Niri user files removed."
say "If Niri still appears in PATH, it’s likely via Nix profiles."
say "• Ensure Home Manager no longer includes:   home.packages = [ niri ];"
say "• Then rebuild:  nix run nixpkgs#home-manager -- switch --flake \"github:oldfart-maker/sysclonev5?dir=home#username\" --refresh -v"
say "• Or remove from your user profile:  nix profile list ; nix profile remove <entry-containing-niri>"
