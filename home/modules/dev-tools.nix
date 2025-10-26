# home/modules/dev-tools.nix
{ pkgs, ... }:
{
  home.packages = with pkgs; [
    cmake
    gcc
    gdb
    ninja
    pkgconf
    clang-tools
    python3
    jq
  ];
}
