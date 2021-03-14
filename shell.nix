let
  sources  = import ./nix/sources.nix;
  commands = import ./nix/commands.nix;

  nixos    = import sources.nixpkgs  {};
  darwin   = import sources.darwin   {};
  unstable = import sources.unstable {};

  pkgs  = if darwin.stdenv.isDarwin then darwin else nixos;
  tasks = commands {
    inherit pkgs;
    inherit unstable;
  };

  deps = {
    common = [
      unstable.niv
    ];

    haskell = [
      unstable.ghcid
      unstable.ghc
      unstable.hlint
      unstable.stack
      unstable.stylish-haskell
      unstable.haskellPackages.hie-bios
      unstable.haskell-language-server
      unstable.haskellPackages.implicit-hie
    ];

    fun = [
      pkgs.figlet
      pkgs.lolcat
    ];
  };

in
  unstable.haskell.lib.buildStackProject {
    name = "rescue";
    nativeBuildInputs = builtins.concatLists [
      deps.common
      deps.haskell
      deps.fun
      tasks
    ];

    shellHook = ''
      export LANG=C.UTF8
    '';
  }
