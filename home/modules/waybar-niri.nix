# modules/waybar-niri.nix
{ config, pkgs, lib, ... }:

let
  # Stylix colors (requires stylix.enable = true)
  ch = config.lib.stylix.colors.withHashtag;

  baseDir = "${config.home.homeDirectory}/.config/niri/waybar";
  cfgPath = "${baseDir}/config";
  cssPath = "${baseDir}/style.css";
  colPath = "${baseDir}/colors.css";
in
{

  # debug marker; safe to remove later
  home.file.".hm-waybar-niri-marker".text = "waybar-niri loaded\n";

  home.packages = [
    pkgs.waybar
    pkgs.pavucontrol
  ];

  # --- colors.css (GTK CSS variables via @define-color) ---
  home.file."${colPath}".text = ''
    @define-color base00 ${ch.base00};
    @define-color base01 ${ch.base01};
    @define-color base02 ${ch.base02};
    @define-color base03 ${ch.base03};
    @define-color base04 ${ch.base04};
    @define-color base05 ${ch.base05};
    @define-color base06 ${ch.base06};
    @define-color base07 ${ch.base07};
    @define-color base08 ${ch.base08};
    @define-color base09 ${ch.base09};
    @define-color base0A ${ch.base0A};
    @define-color base0B ${ch.base0B};
    @define-color base0C ${ch.base0C};
    @define-color base0D ${ch.base0D};
    @define-color base0E ${ch.base0E};
    @define-color base0F ${ch.base0F};

    @define-color bg       ${ch.base00};
    @define-color bg_alt   ${ch.base01};
    @define-color bg_alt2  ${ch.base02};
    @define-color fg       ${ch.base05};

    @define-color red      ${ch.base08};
    @define-color orange   ${ch.base09};
    @define-color yellow   ${ch.base0A};
    @define-color green    ${ch.base0B};
    @define-color teal     ${ch.base0C};
    @define-color blue     ${ch.base0D};
    @define-color mauve    ${ch.base0E};
    @define-color accent   ${ch.base0D};
  '';

  # --- style.css (imports the palette above) ---
  home.file."${cssPath}".text = ''
    @import url("colors.css");

    /* ---- global ---- */
    * {
      font-family: "JetBrains Mono", "Symbols Nerd Font", "Noto Color Emoji", monospace, sans-serif;
      font-size: 10pt;
      color: @fg;
    }

    window#waybar {
      background: alpha(@bg, 0.50);   /* subtle transparency */
      border-radius: 12px;
      padding: 6px;
      box-shadow: 0 8px 24px alpha(@fg, 0.06);
    }

    /* block groups */
    .modules-left,
    .modules-center,
    .modules-right {
      margin: 0 6px;
    }

    /* ---- module "pill" look ---- */
    .module {
      padding: 4px 12px;
      margin: 0 6px;
      border-radius: 10px;
      background: alpha(@bg_alt, 0.9);
    }

    .module:hover { background: alpha(@bg_alt2, 0.9); }

    /* separators (optional) */
    /*
    .module + .module {
      border-left: 1px solid alpha(@fg, 0.08);
    }
    */

    /* ---- per-module accents ---- */
    #clock      { color: @accent; }
    #network    { color: @blue; }
    #pulseaudio { color: @mauve; }
    #backlight  { color: @yellow; }
    #cpu        { color: @orange; }
    #memory     { color: @teal; }
    #tray       { color: @fg; }

    /* pulseaudio muted state pop */
    #pulseaudio.muted {
      color: @red;
      background: alpha(@red, 0.10);
    }

    /* network disconnected */
    #network.disconnected {
      color: @orange;
      background: alpha(@orange, 0.10);
    }

    /* tray cleanup */
    #tray { padding-right: 8px; }
    #tray > * { margin: 0 4px; }

    /* --- workspaces --- */
    #workspaces {
      padding: 0 6px;
    }

    #workspaces button {
      padding: 2px 8px;
      margin: 0 4px;
      border-radius: 8px;
      background: alpha(@bg_alt, 0.8);
    }

    #workspaces button.focused {
      color: @accent;
      background: alpha(@accent, 0.12);
    }

    #workspaces button.active { background: alpha(@blue, 0.10); }
    #workspaces button.urgent { background: alpha(@red, 0.12); }
    #workspaces button.empty  { opacity: .55; }

  '';

  # --- Waybar JSON config ---
  home.file."${cfgPath}".text = builtins.toJSON {
    layer = "top";
    position = "top";
    height = 28;
    margin = "6 6 0 6";
    spacing = 8;

    # add workspaces to the left group (put it first if you want it leading)
    "modules-left"   = [ "niri/workspaces" "cpu" "memory" ];
    "modules-center" = [ "clock" ];
    "modules-right"  = [ "pulseaudio" "backlight" "network" "tray" ];

    # NEW: niri workspaces module
    "niri/workspaces" = {
      # show an icon per workspace; click switches workspaces
      format = "{icon}";
      "format-icons" = {
        # state-based icons
        active  = "";
        default = "";
        urgent  = "";
        empty   = "";
        # (optional) name/index matches also supported if you name workspaces in niri
        # "browser" = "";
        # "chat"    = "";
      };
      # show all workspaces on every output (set to false to only show for that monitor)
      "all-outputs" = true;
      "disable-click" = false;
      # "expand" = false;  # set true if you want it to stretch
    };

    clock = {
      format = "{:%a %b %d  %H:%M}";
      tooltip = true;
      tooltip-format = "{:%A, %B %d, %Y}";
    };

    network = {
      format-wifi         = "  {essid} {signalStrength}%";
      format-ethernet     = "  {ifname}";
      format-disconnected = "󰤮  down";
      tooltip = true;
    };

    pulseaudio = {
      scroll-step = 1;
      format = "{icon}  {volume}%";
      format-muted = "󰝟  mute";
      format-icons = { default = [ "󰕿" "󰖀" "󰕾" ]; headphone = "󰋋"; };
      on-click = "pavucontrol || true";
    };

    backlight = { format = "󰖨  {percent}%"; };
    cpu      = { interval = 3; format = "  {usage}%"; tooltip = false; };
    memory   = { interval = 5; format = "  {used:0.1f}G"; tooltip = false; };
    tray     = { spacing = 8; };
  };
}
