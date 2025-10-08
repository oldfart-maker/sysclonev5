{ config, pkgs, lib, ... }:
let
  emacsPkg = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk);
in {
  # Put the binaries on PATH (repro across shells)
  home.packages = [ emacsPkg ];

  # Run the daemon (repro across boots)
  services.emacs = {
    enable = true;
    package = emacsPkg;
  };

  # Optional, nice-to-have reproducibility for tooling
  programs.git.enable = true;
  # programs.bash.initExtra = ''export EDITOR=emacsclient'';
}
