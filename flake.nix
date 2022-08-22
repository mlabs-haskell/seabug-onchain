{
  description = "seabug-onchain";

  inputs = {
    haskell-nix.follows = "plutip/bot-plutus-interface/haskell-nix";
    nixpkgs.follows = "plutip/bot-plutus-interface/haskell-nix/nixpkgs";
    iohk-nix.follows = "plutip/bot-plutus-interface/iohk-nix";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    plutip.url =
      "github:mlabs-haskell/plutip?rev=1ee56e97d3c00bf125b5c4657ef9345a14b8dfc7";
  };

  outputs = { self, plutip, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      haskellModules = inputs.plutip.haskellModules;

      extraSources = inputs.plutip.extraSources ++ [{
        src = inputs.plutip;
        subdirs = [ "." ];
      }];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.haskell-nix.cabalProject {
          name = "seabug-onchain";
          src = ./.;
          compiler-nix-name = "ghc8107";
          shell = {
            additional = ps:
              [
                ps.plutus-pab
                ps.bot-plutus-interface
                ps.plutus-use-cases
                ps.plutip
              ];
            withHoogle = true;
            tools.haskell-language-server = { };
            exactDeps = true;
            nativeBuildInputs = with pkgs'; [
              cabal-install
              haskellPackages.cabal-fmt
              haskellPackages.implicit-hie
              haskellPackages.fourmolu
              hlint
              jq
              websocat
              fd
              nixpkgs-fmt
            ];
          };
          inherit (plutip) cabalProjectLocal;
          inherit extraSources;
          modules = haskellModules;
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          { nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ]; } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          mkdir $out
        '';

    in
    {
      inherit extraSources haskellModules;
      inherit (plutip) cabalProjectLocal;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "seabug-onchain:lib:seabug-onchain";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [ self.devShell.${system}.inputDerivation ];
          } "touch $out");
      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
      });

      hydraJobs.x86_64-linux =
        self.checks.x86_64-linux // self.packages.x86_64-linux;
    };
}
