{ src, inputs, pkgs, system, doCoverage ? false, deferPluginErrors ? true, ... }:

pkgs.haskell-nix.cabalProject {
  inherit src;

  name = "seabug-onchain";

  compiler-nix-name = "ghc8107";

  shell = {
    inputsFrom = [ pkgs.libsodium-vrf ];

    # Make sure to keep this list updated after upgrading git dependencies!
    additional = ps:
      with ps; [
        bot-plutus-interface
        filemanip
        ieee
        plutus-extra
        tasty-plutus
        plutus-pretty
        plutus-laws
        plutus-numeric
        base-deriving-via
        cardano-addresses
        cardano-addresses-cli
        cardano-binary
        cardano-crypto
        cardano-crypto-class
        cardano-crypto-praos
        cardano-crypto-wrapper
        cardano-ledger-alonzo
        cardano-ledger-byron
        cardano-ledger-core
        cardano-ledger-pretty
        cardano-ledger-shelley
        cardano-ledger-shelley-ma
        cardano-prelude
        cardano-slotting
        flat
        freer-extras
        goblins
        measures
        orphans-deriving-via
        playground-common
        plutus-chain-index
        plutus-ledger-constraints
        plutus-contract
        plutus-core
        plutus-ledger
        plutus-ledger-api
        plutus-pab
        plutus-playground-server
        plutus-tx
        plutus-tx-plugin
        plutus-tx-spooky
        # plutus-simple-model
        plutus-use-cases
        # plutip
        prettyprinter-configurable
        quickcheck-dynamic
        Win32-network
        word-array
      ];

    withHoogle = true;

    tools = {
      cabal = "latest";
      haskell-language-server = "latest";
    };

    exactDeps = true;

    nativeBuildInputs = with pkgs;
      [
        # Haskell Tools
        haskellPackages.fourmolu
        hlint
        entr
        ghcid
        git

        # hls doesn't support preprocessors yet so this has to exist in PATH
        haskellPackages.record-dot-preprocessor

        # Graphviz Diagrams for documentation
        graphviz
        pkg-config
        libsodium-vrf
      ] ++ (lib.optionals (!stdenv.isDarwin) [
        rPackages.plotly
        R
        systemdMinimal
      ]);
  };

  modules = [{
    packages = {
      eventful-sql-common.doHaddock = false;
      eventful-sql-common.ghcOptions = [''
        -XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
                -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses''];

      plutus-use-cases.doHaddock = deferPluginErrors;
      plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

      plutus-contract.doHaddock = deferPluginErrors;
      plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

      plutus-ledger.doHaddock = deferPluginErrors;
      plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

      # plutus-simple-model.doHaddock = false;
      # plutus-simple-model.flags.defer-plugin-errors = deferPluginErrors;

      # see https://github.com/input-output-hk/haskell.nix/issues/1128
      ieee.components.library.libs = pkgs.lib.mkForce [ ];

      cardano-crypto-praos.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      cardano-crypto-class.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      cardano-wallet-core.components.library.build-tools =
        [ pkgs.buildPackages.buildPackages.gitMinimal ];
      cardano-config.components.library.build-tools =
        [ pkgs.buildPackages.buildPackages.gitMinimal ];

      seabug-onchain.components.tests."seabug-onchain-tests".build-tools =
        [ inputs.cardano-node.packages.${system}.cardano-node
          inputs.cardano-node.packages.${system}.cardano-cli
        ];

    };
  }];

  extraSources = [
    {
      src = inputs.cardano-addresses;
      subdirs = [ "core" "command-line" ];
    }
    {
      src = inputs.cardano-base;
      subdirs = [
        "base-deriving-via"
        "binary"
        "binary/test"
        "cardano-crypto-class"
        "cardano-crypto-praos"
        "cardano-crypto-tests"
        "measures"
        "orphans-deriving-via"
        "slotting"
        "strict-containers"
      ];
    }
    {
      src = inputs.cardano-crypto;
      subdirs = [ "." ];
    }
    {
      src = inputs.cardano-ledger;
      subdirs = [
        "eras/alonzo/impl"
        "eras/byron/chain/executable-spec"
        "eras/byron/crypto"
        "eras/byron/crypto/test"
        "eras/byron/ledger/executable-spec"
        "eras/byron/ledger/impl"
        "eras/byron/ledger/impl/test"
        "eras/shelley/impl"
        "eras/shelley-ma/impl"
        "eras/shelley/test-suite"
        "libs/cardano-data"
        "libs/cardano-ledger-core"
        "libs/cardano-ledger-pretty"
        "libs/cardano-protocol-tpraos"
        "libs/compact-map"
        "libs/non-integral"
        "libs/set-algebra"
        "libs/small-steps"
        "libs/small-steps-test"
      ];
    }
    {
      src = inputs.cardano-node;
      subdirs = [ "cardano-api" ];
    }
    {
      src = inputs.cardano-prelude;
      subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
    }
    {
      src = inputs.cardano-wallet;
      subdirs = [
        "lib/cli"
        "lib/core"
        "lib/core-integration"
        "lib/dbvar"
        "lib/launcher"
        "lib/numeric"
        "lib/shelley"
        "lib/strict-non-empty-containers"
        "lib/test-utils"
        "lib/text-class"
      ];
    }
    {
      src = inputs.flat;
      subdirs = [ "." ];
    }
    {
      src = inputs.goblins;
      subdirs = [ "." ];
    }
    {
      src = inputs.iohk-monitoring-framework;
      subdirs = [
        "iohk-monitoring"
        "tracer-transformers"
        "contra-tracer"
        "plugins/backend-aggregation"
        "plugins/backend-ekg"
        "plugins/backend-monitoring"
        "plugins/backend-trace-forwarder"
        "plugins/scribe-systemd"
      ];
    }
    {
      src = inputs.optparse-applicative;
      subdirs = [ "." ];
    }
    {
      src = inputs.ouroboros-network;
      subdirs = [
        "io-classes"
        "io-sim"
        "monoidal-synchronisation"
        "network-mux"
        "ntp-client"
        "ouroboros-consensus"
        "ouroboros-consensus-byron"
        "ouroboros-consensus-cardano"
        "ouroboros-consensus-protocol"
        "ouroboros-consensus-shelley"
        "ouroboros-network"
        "ouroboros-network-framework"
        "ouroboros-network-testing"
        "strict-stm"
        "typed-protocols"
        "typed-protocols-cborg"
        "typed-protocols-examples"
      ];
    }
    {
      src = inputs.plutus;
      subdirs = [
        "plutus-core"
        "plutus-ledger-api"
        "plutus-tx"
        "plutus-tx-plugin"
        "prettyprinter-configurable"
        "stubs/plutus-ghc-stub"
        "word-array"
      ];
    }
    {
      src = inputs.plutus-apps;
      subdirs = [
        "doc"
        "freer-extras"
        "playground-common"
        "plutus-chain-index"
        "plutus-chain-index-core"
        "plutus-contract"
        "plutus-ledger-constraints"
        "plutus-ledger"
        "plutus-pab"
        "plutus-playground-server"
        "plutus-use-cases"
        "quickcheck-dynamic"
        "web-ghc"
      ];
    }
    {
      src = inputs.plutus-extra;
      subdirs = [
        "plutus-extra"
        "tasty-plutus"
        "plutus-pretty"
        "plutus-numeric"
        "plutus-golden"
        "plutus-laws"
        "plutus-list"
        # "plutus-size-check"
        "quickcheck-plutus-instances"
        "plutus-deriving"
      ];
    }
    {
      src = inputs.plutus-tx-spooky;
      subdirs = [ "." ];
    }
    # {
    #   src = inputs.plutus-simple-model;
    #   subdirs = [ "." ];
    # }
    {
      src = inputs.purescript-bridge;
      subdirs = [ "." ];
    }
    {
      src = inputs.servant-purescript;
      subdirs = [ "." ];
    }
    {
      src = inputs.Win32-network;
      subdirs = [ "." ];
    }
    # {
    #   src = inputs.plutip;
    #   subdirs = [ "." ];
    # }
    {
      src = inputs.bot-plutus-interface;
      subdirs = [ "." ];
    }
  ];
}