# home/modules/niri.nix
{ config, lib, ... }:

let
  repoRoot   = "${config.home.homeDirectory}/projects/sysclonev5";
  genNiriDir = "${repoRoot}/generated/niri";
  cfgSrc     = "${genNiriDir}/config.kdl";
  keysSrc    = "${genNiriDir}/key_bindings.txt";
  niriDir    = "${config.xdg.configHome}/niri";
in
{
  # write the exact bytes from your repo into the target file
  home.file."${niriDir}/config.kdl".text = builtins.readFile cfgSrc;

  # optional keybindings: only if present
  home.activation.niriOptionalKeymap = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -Eeuo pipefail
    if [ -f "${keysSrc}" ]; then
      install -m 0644 -D "${keysSrc}" "${niriDir}/key_bindings.txt"
    fi
  '';

  # fail fast if the config wasn’t generated/pulled yet
  home.activation.niriGeneratedCheck = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -Eeuo pipefail
    if [ ! -f "${cfgSrc}" ]; then
      echo "[niri] ERROR: ${cfgSrc} not found."
      echo "[niri] Run on host: tools/tangle-sync.sh --niri && git push"
      echo "[niri] Then on the Pi: hm-update"
      exit 40
    fi
  '';
}
