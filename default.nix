{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

  overrides = self: super: let
    diagrams-reflex = pkgs.fetchFromGitHub {
      owner = "diagrams";
      repo = "diagrams-reflex";
      rev = "4121332f3ddfcc900898a8a57d2e47a8ea6be974";
      sha256 = "0ia3lnhg127vf15ynlfw6mw01nh0j0y5q5sjxd3mi3v8aa4dr2wm";
    };

    reflex-dom-contrib = pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "b47f90c810c838009bf69e1f8dacdcd10fe8ffe3";
      sha256 = "0yvjnr9xfm0bg7b6q7ssdci43ca2ap3wvjhshv61dnpvh60ldsk9";
    };

    semantic-reflex = pkgs.fetchFromGitHub {
      owner = "tomsmalley";
      repo = "semantic-reflex";
      rev = "7946d5f43ffce2c4cb090e315bb391af86e66276";
      sha256 = "0c18ylawg9xnj12hahzp3nwf7vs05rjpvdmwk1nna5z8yy661xkz";
    };

    servant-reflex = pkgs.fetchFromGitHub {
      owner = "imalsogreg";
      repo = "servant-reflex";
      rev = "2996dbc8e0922e29939d05ac647b897650ab64a8";
      sha256 = "03880y11yfmmxk1s35wdqzghm4pcq44qm4hcw0577mmi4gdva8vi";
    };

    servant-auth = pkgs.fetchFromGitHub {
      owner = "haskell-servant";
      repo = "servant-auth";
      rev = "293998f6212835f5d755b0182868d7531141110a";
      sha256 = "1hsxnlm909fs7cawgdnrn6nygf9shzr9n05xi5b52bq7jgpvhwbq";
    };

    glob = pkgs.fetchFromGitHub {
      owner = "Deewiant";
      repo = "glob";
      rev = "d55ac01a8a98efc6ab881acc56b4d0fc6595019d";
      sha256 = "02i64r7zrab2b4kyf5z3fq8hvljf7s6gzd8whdvdsjik7vwyzcd1";
    };

    sv = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "sv";
      rev = "ce417c0ea666717ff95eb45709eea0f1778d9c67";
      sha256 = "1makl6djfknh79fwvbjixgjx5hvkqsmq7z8x8gqifxlidhjp8kf6";
    };

    separated = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "separated";
      rev = "0.3.2.1-nix";
      sha256 = "07f3nh1b4jvqq7lfyxp3ndgzap4dj31lbdjlgrjazrcd2h4zwdln";
    };

    validation = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "validation";
      rev = "1";
      sha256 = "0bh3i6dkkxc6sxzbdwk9hkyyqm9cvx7261vl7zrxk0myrj2klfbr";
    };

    hedgehog = pkgs.fetchFromGitHub {
      owner  = "hedgehogqa";
      repo   = "haskell-hedgehog";
      rev    = "7858d626b198621bc674fbc235c7980fb4002f78";
      sha256 = "0mmypd6f3imh7bk6br9m9aj97k2yibz2bqcw3a5svp962zsjbkyp";
    };

    dhall-haskell = pkgs.fetchFromGitHub {
      owner = "dhall-lang";
      repo = "dhall-haskell";
      rev = "7d7b8590b5ef82598bbf36e146a0c4c20e4dec04";
      sha256 = "0kywaizrp4d7l7qfq0af70m95qn8mv5dkd1mw50vpjx3b8mar59h";
    };

    formatting = pkgs.fetchFromGitHub {
      owner = "chrisdone";
      repo = "formatting";
      rev = "14f04ce3fadbf7a0c3d2a7fe0b35f154de9f7c39";
      sha256 = "1n5mfcxwxrk177x1gsllyc7dsks8jn5wvs4sb8hj7pawgq1frkbp";
    };

    prettyprinter = pkgs.fetchFromGitHub {
      owner = "quchen";
      repo = "prettyprinter";
      rev = "6a45f4ebe1087cd1939554fc79d49a8c073f3cd6";
      sha256 = "0phzm34figj1c1h9s9cdhkdwaz7ascf7gfkclr3lccmp579z355z";
    };
  in
  {
    dhall = self.callCabal2nix "dhall" "${dhall-haskell}" {};
    formatting = self.callCabal2nix "formatting" "${formatting}" {};
    # prettyprinter = self.callCabal2nix "prettyprinter" "${prettyprinter}/prettyprinter" {};
    prettyprinter = self.callPackage "${dhall-haskell}/nix/prettyprinter.nix" {};

    diagrams-reflex = self.callPackage "${diagrams-reflex}" {};
    reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" "${reflex-dom-contrib}" {};
    semantic-reflex = self.callCabal2nix "semantic-reflex" "${semantic-reflex}/semantic-reflex" {};
    servant-reflex = self.callCabal2nix "servant-reflex" "${servant-reflex}" {};
    servant-auth = self.callCabal2nix "servant-auth" "${servant-auth}/servant-auth" {};
    Glob = self.callCabal2nix "Glob" "${glob}" {};
    # validation = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.validation;

    # sv
    parsers = pkgs.haskell.lib.dontCheck super.parsers;
    separated = pkgs.haskell.lib.dontCheck (self.callCabal2nix "separated" "${separated}" {});
    validation = pkgs.haskell.lib.dontCheck (self.callCabal2nix "validation" "${validation}" {});
    hedgehog = super.callCabal2nix "hedgehog" "${hedgehog}/hedgehog" {};
    sv = self.callCabal2nix "sv" "${sv}" {};
  };
})
