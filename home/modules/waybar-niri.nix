# home/modules/waybar.nix
{ config, pkgs, lib, ... }:

let c = config.lib.stylix.colors.withHashtag; in
{
  programs.waybar = {
    enable = true;
    package = pkgs.waybar;

    settings = [
      {
        layer = "top";
        position = "top";
        height = 28;
        "gtk-layer-shell" = true;

        "modules-left"   = [ "niri/workspaces" ];
        "modules-center" = [ "clock" ];
        "modules-right"  = [ "backlight" "pulseaudio" "cpu" "memory" "tray" ];

        "niri/workspaces" = { "disable-scroll" = true; };
        "clock" = { interval = 1; format = "{:%a %b %d  %H:%M:%S}"; tooltip = false; };
        "pulseaudio" = {
          format = "{icon}  {volume}%";
          "format-muted" = "󰝟  mute";
          "format-icons" = { default = [ "󰕿" "󰖀" "󰕾" ]; headphone = "󰋋"; };
          "on-click" = "pavucontrol || true";
        };
        "backlight" = { format = "󰖨  {percent}%"; };
        "cpu"      = { interval = 3; format = "  {usage}%"; tooltip = false; };
        "memory"   = { interval = 5; format = "  {used:0.1f}G"; tooltip = false; };
        "tray"     = { spacing = 8; };
      }
    ];

    style = ''
      :root {
        --bg: ${c.base00}; --bg-alt: ${c.base01};
        --fg: ${c.base05}; --fg-dim: ${c.base04};
        --accent: ${c.base0D};
      }
      * { font-family: "JetBrains Mono","Symbols Nerd Font",monospace; font-size: 12pt; }
      window#waybar { background: var(--bg); color: var(--fg); }
      .modules-left, .modules-center, .modules-right { padding: 6px 10px; }
      #workspaces button { padding: 0 8px; margin: 0 4px; color: var(--fg-dim); }
      #workspaces button.active { color: var(--fg); background: rgba(255,255,255,.05); border-radius: 8px; }
      #cpu, #memory, #backlight, #pulseaudio, #clock, #tray { margin: 0 6px; }
    '';
  };
}
