{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # build stack
    cmake
    gnumake
    gcc
    ninja
    pkgconf
    which
    libtool
    autoconf
    automake
    gdb
    clang-tools
    python3
    jq
  ];
}
