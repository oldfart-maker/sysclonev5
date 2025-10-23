set -euo pipefail

# ----- config -----
repo="$HOME/projects/sysclonev5"
flake="path:$repo/home#username"

echo ">>> STEP 1: uninstall HM (removes user-profile entry)"
home-manager uninstall || true

echo ">>> STEP 2: remove HM profile links/generations (user)"
rm -f  "$HOME/.local/state/nix/profiles/home-manager" 2>/dev/null || true
rm -rf "$HOME/.local/state/nix/profiles/home-manager-*" 2>/dev/null || true
rm -rf "$HOME/.home-manager" 2>/dev/null || true   # legacy path if it exists
rm -rf "$HOME/.cache/hm" 2>/dev/null || true
rm -rf "$repo/home/result-activate" 2>/dev/null || true

echo ">>> STEP 3: clear any user profile entries referencing HM"
# Not all installs use this, but it’s harmless if empty:
nix profile list || true
# Try to remove by name; ignore errors if it’s not present.
nix profile remove --profile "$HOME/.local/state/nix/profile" home-manager-path 2>/dev/null || true
# As an alternative sledgehammer (wipes user profile history):
# nix profile wipe-history --profile "$HOME/.local/state/nix/profile" --older-than 0d || true

echo ">>> STEP 4: remove stale flake registry pin (prevents old GitHub revs)"
nix registry list | grep -i sysclonev5 || true
nix registry remove "github:oldfart-maker/sysclonev5" 2>/dev/null || true

echo ">>> STEP 5: GC store (user) — doesn’t delete your home files"
nix store gc || true
# If you have multi-user Nix and want to be thorough, you can also:
# sudo nix-collect-garbage -d || true
# Optional integrity pass (only if you suspect real corruption):
# sudo nix store verify --repair || true

echo ">>> STEP 6: hard-sync local repo to origin/main"
cd "$repo"
git fetch --prune origin
git checkout -B main origin/main
git reset --hard origin/main
git submodule update --init --recursive
echo "Building commit: $(git rev-parse --short HEAD)"

echo ">>> STEP 7: fresh HM switch from LOCAL PATH flake"
nix run nixpkgs#home-manager -- switch --flake "$flake" --refresh -v

echo ">>> STEP 8: quick sanity"
head -n 6 "$HOME/.local/bin/hm-update" 2>/dev/null || echo "no hm-update (ok if your config doesn’t generate it)"
ls -l "$HOME/.config/niri/config.kdl" 2>/dev/null || echo "no niri/config.kdl yet"
