# modules/waybar.nix
{ config, pkgs, lib, ... }:

let
  c  = config.lib.stylix.colors;
  ch = config.lib.stylix.colors.withHashtag;
in
{
  programs.waybar = {
    enable = true;
    systemd.enable = true;

    # JSON settings (Waybar 0.10+ supports JSONC if you prefer; HM uses JSON)
    settings = [
      {
        layer = "top";
        position = "top";
        height = 28;
        margin = "6 6 0 6";
        spacing = 8;

        "modules-left"   = [ "cpu" "memory" ];
        "modules-center" = [ "clock" ];
        "modules-right"  = [ "pulseaudio" "backlight" "network" "battery" "tray" ];

        clock = {
          format = "{:%a %b %d  %H:%M}";
          tooltip = true;
          tooltip-format = "{:%A, %B %d, %Y}";
        };

        network = {
          format-wifi = "  {essid} {signalStrength}%";
          format-ethernet = "  {ifname}";
          format-disconnected = "󰤮  down";
          tooltip = true;
        };

        battery = {
          states = { warning = 20; critical = 10; };
          format = "{icon}  {capacity}%";
          format-charging = "󰂄  {capacity}%";
          format-plugged  = "  {capacity}%";
          format-icons = [ "󰁺" "󰁼" "󰁾" "󰂀" "󰂂" "󰁹" ];
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
      }
    ];

    # Our main CSS. It imports colors.css (generated below).
    style = ''
      @import url("colors.css");

      * {
        font-family: Inter, JetBrainsMono, monospace;
        font-size: 12pt;
      }

      window#waybar {
        background: var(--bg);
        color: var(--fg);
        border-radius: 10px;
      }

      .module {
        padding: 2px 10px;
        border-radius: 8px;
        background: var(--bg-alt);
      }

      .module:hover {
        background: var(--bg-alt-2);
      }

      #clock      { color: var(--accent); }
      #network    { color: var(--blue); }
      #battery    { color: var(--green); }
      #pulseaudio { color: var(--mauve); }
      #backlight  { color: var(--yellow); }
      #cpu        { color: var(--peach); }
      #memory     { color: var(--teal); }
      #tray       { color: var(--fg); }

      #battery.warning  { color: var(--yellow); }
      #battery.critical { color: var(--red); }
    '';
  };

  # Export Stylix → CSS variables for Waybar
  home.file.".config/waybar/colors.css".text = ''
    :root {
      --base00: ${ch.base00};
      --base01: ${ch.base01};
      --base02: ${ch.base02};
      --base03: ${ch.base03};
      --base04: ${ch.base04};
      --base05: ${ch.base05};
      --base06: ${ch.base06};
      --base07: ${ch.base07};
      --base08: ${ch.base08};
      --base09: ${ch.base09};
      --base0A: ${ch.base0A};
      --base0B: ${ch.base0B};
      --base0C: ${ch.base0C};
      --base0D: ${ch.base0D};
      --base0E: ${ch.base0E};
      --base0F: ${ch.base0F};

      --bg:       var(--base00);
      --bg-alt:   var(--base01);
      --bg-alt-2: var(--base02);
      --fg:       var(--base05);

      --red:    var(--base08);
      --orange: var(--base09);
      --yellow: var(--base0A);
      --green:  var(--base0B);
      --teal:   var(--base0C);
      --blue:   var(--base0D);
      --mauve:  var(--base0E);
      --peach:  var(--base09);
      --accent: var(--base0D);
    }
  '';
}
