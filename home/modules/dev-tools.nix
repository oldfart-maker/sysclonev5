# home/modules/dev-tools.nix
{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # build tools
    cmake
    gnumake
    gcc
    ninja
    pkgconf
    which
    gdb
    clang-tools
    python3
    jq
  ];
}
