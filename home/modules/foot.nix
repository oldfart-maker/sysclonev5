# in home/modules/foot.nix
{ config, pkgs, lib, ... }:

let
  c = (config.lib.stylix.colors or {
    base00 = "#000000"; base03 = "#333333"; base05 = "#d0d0d0"; base07 = "#ffffff";
    base08 = "#ff5f5f"; base0A = "#ffd75f"; base0B = "#87d787"; base0C = "#5fd7d7";
    base0D = "#5f87ff"; base0E = "#af5fff";
  });
  footDir = "${config.xdg.configHome}/niri/foot";
  footIni = "${footDir}/foot.ini";
in
{
home.activation._debugFootPing = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  echo "[foot] foot.nix module ACTIVE"
  mkdir -p "${config.xdg.configHome}/niri/foot"
  date > "${config.xdg.configHome}/niri/foot/FOOT_MODULE_ACTIVE"
'';

  # guaranteed write each switch
  home.activation.writeFootIni = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -Eeuo pipefail
    mkdir -p "${footDir}"
    cat > "${footIni}" <<'INI'
[main]
font=JetBrains Mono:size=11
font-bold=JetBrains Mono:style=Bold:size=11
font-italic=JetBrains Mono:style=Italic:size=11
font-bold-italic=JetBrains Mono:style=Bold Italic:size=11
line-height=1.1

[colors]
background=${c.base00}
foreground=${c.base05}
regular0=${c.base00}
regular1=${c.base08}
regular2=${c.base0B}
regular3=${c.base0A}
regular4=${c.base0D}
regular5=${c.base0E}
regular6=${c.base0C}
regular7=${c.base05}
bright0=${c.base03}
bright1=${c.base08}
bright2=${c.base0B}
bright3=${c.base0A}
bright4=${c.base0D}
bright5=${c.base0E}
bright6=${c.base0C}
bright7=${c.base07}
INI
  '';
}
