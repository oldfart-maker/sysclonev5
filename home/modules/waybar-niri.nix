# home/modules/waybar-niri.nix
{ config, pkgs, lib, ... }:

let
  c = config.lib.stylix.colors.withHashtag;
in
{
  home.packages = [ pkgs.waybar pkgs.pavucontrol ];

  # Colors → CSS variables
  xdg.configFile."niri/waybar/colors.css".text = ''
    :root {
      --bg: ${c.base00};
      --bg-alt: ${c.base01};
      --fg: ${c.base05};
      --fg-dim: ${c.base04};
      --accent: ${c.base0D};
      --warn: ${c.base0A};
      --crit: ${c.base08};
      --ok: ${c.base0B};
    }
  '';

  # Style (feel free to tweak)
  xdg.configFile."niri/waybar/style.css".text = ''
    @import url("colors.css");

    * { font-family: "JetBrains Mono", "Symbols Nerd Font", monospace;
        font-size: 12pt; }

    window#waybar {
      background: var(--bg);
      color: var(--fg);
      border: 0;
    }

    .modules-left, .modules-center, .modules-right { padding: 6px 10px; }

    #workspaces button {
      padding: 0 8px; margin: 0 4px;
      color: var(--fg-dim);
    }
    #workspaces button.active {
      color: var(--fg); background: rgba(255,255,255,0.05); border-radius: 8px;
    }

    #cpu, #memory, #backlight, #pulseaudio, #clock, #tray {
      margin: 0 6px;
    }
  '';

  # Config (JSON; uses Waybar’s niri modules)
  xdg.configFile."niri/waybar/config".text = lib.generators.toJSON {} {
    layer = "top";
    position = "top";
    height = 28;
    gtk-layer-shell = true;

    modules-left   = [ "niri/workspaces" ];
    modules-center = [ "clock" ];
    modules-right  = [ "backlight" "pulseaudio" "cpu" "memory" "tray" ];

    "niri/workspaces" = {
      # show all monitors’ workspaces, or set "persistent-workspaces"
      "disable-scroll" = true;
    };

    clock = {
      interval = 1;
      format = "{:%a %b %d  %H:%M:%S}";
      tooltip = false;
    };

    pulseaudio = {
      format = "{icon}  {volume}%";
      "format-muted" = "󰝟  mute";
      "format-icons" = { default = [ "󰕿" "󰖀" "󰕾" ]; headphone = "󰋋"; };
      "on-click" = "pavucontrol || true";
    };

    backlight = { format = "󰖨  {percent}%"; };
    cpu      = { interval = 3; format = "  {usage}%"; tooltip = false; };
    memory   = { interval = 5; format = "  {used:0.1f}G"; tooltip = false; };
    tray     = { spacing = 8; };
  };
}
