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
      rev = "b6075f6b052de5071b16c9d29526dda44ea4c092";
      sha256 = "1scwwcvj0ycd6zd5i01d1w03c4xifrrs5ywwyylp38fqz62zm21g";
    };

    # Not using v0.13 as they require latest http-types which jsaddle doesn't work with.
    servant= pkgs.fetchFromGitHub {
      owner = "haskell-servant";
      repo = "servant";
      rev = "7e9910b27ee42c635bb71f87bbdaa6056ab22c23";
      # rev = "1d55429f25ca28b750fd52926e4321cfe19dbf0e"; # v0.13
      sha256 = "0j2j6x8wdc3jzpixaq4wrpb4873bj4jvqmfbhcybdqx8cl8v36yp";
    };

    aeson = pkgs.fetchFromGitHub {
      owner = "bos";
      repo = "aeson";
      # Pinning aeson<1.2 for pandoc.
      rev = "3e5b02876c36d7471534c558b61ee5ff0e705a96";
      sha256 = "1n29pd2is0w4d8g037r8hl2xpdi60d34i803xps1wvp0r8x90d0w";
      # rev = "8e58f82db806424ea3690ed1637375f2aadc7940";
      # sha256 = "0i62cx7i26zhr93cbajkv4qh3d7f00wxp06j6wy67xsmrf44m1dm";
    };

    attoparsec = pkgs.fetchFromGitHub {
      owner = "bos";
      repo = "attoparsec";
      rev = "c8030ed56df344b4c7238e58c5349e64f70c7bc9";
      sha256 = "1v94nwkm050vwhsv55rvw0d8syvivx7f6b6wz04lss3pfcrj4qwl";
    };

    base-compat = pkgs.fetchFromGitHub {
      owner = "haskell-compat";
      repo = "base-compat";
      rev = "877917365da629da7f76afa35ff99524504603dd";
      sha256 = "1nski1fg9ba9xadr57656lj1w01lq95ljcf6m6zq4mm3qdm0x3nh";
    };

    http-media = pkgs.fetchFromGitHub {
      owner = "zmthy";
      repo = "http-media";
      rev = "501f8c0b90fe2f1675f7d19f32bc7c5b4a67c7bb";
      sha256 = "1rjxk0r09xh2m2f8wyzhhvmmdfmlbmn7s126vgkdxkqanarf0m46";
    };

    http-types = pkgs.fetchFromGitHub {
      owner = "aristidb";
      repo = "http-types";
      rev = "7461df39467dc1dd2bb9697c50f186d196551dde";
      sha256 = "0law0hwspwgy2hga7zphmaqqbfgklfpb2pf813ycgqnq1fzaai0m";
    };

    mmorph = pkgs.fetchFromGitHub {
      owner = "Gabriel439";
      repo = "Haskell-MMorph-Library";
      rev = "c557fd52358d15c395c33b63a2e7e318160d735c";
      sha256 = "0a96q893zzj8zsq815qzmk341ykjdk7qh8rpp541hj40f53k55ir";
    };

    text = pkgs.fetchFromGitHub {
      owner = "haskell";
      repo = "text";
      rev = "a02c2dafafa425bd5f36c8629e98b98daf1cfa1e";
      sha256 = "0rh9mb023f0s56ylzxz9c3c1y09lpl6m69ap5bnpdi0dz7fm6s85";
    };

    # Forked servant-reflex to relax cabal version constraints
    servant-reflex = pkgs.fetchFromGitHub {
      owner = "srid";
      repo = "servant-reflex";
      rev = "7ad155b6ed07ef23bfe57235bd5ccfb60b22bcfb";
      # upstream  - rev = "2996dbc8e0922e29939d05ac647b897650ab64a8";
      # sha256 = "03880y11yfmmxk1s35wdqzghm4pcq44qm4hcw0577mmi4gdva8vi";
      sha256 = "1r829j3qy7vwcng7xpwfp2w5605i43w5x8g5skgd1iz7a5mfmq5i";
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

    # Pinning to PR <https://github.com/dhall-lang/dhall-haskell/issues/346> until it is merged.
    dhall-haskell = pkgs.fetchFromGitHub {
      owner = "dhall-lang";
      repo = "dhall-haskell";
      rev = "19d89c2e3c4189ab757a05d8e4d201e867efe318";
      sha256 = "0sb8qm9glnlbamnbkvzixirmxgj4zz8wpqixklwl26sbjhb93sfx";
    };

    megaparsec = pkgs.fetchFromGitHub {
      owner = "mrkkrp";
      repo = "megaparsec";
      rev = "20646728cc9e8a073b7793b12865bba95b61d29e";
      sha256 = "1728s0rm1mqh1104bh2s7m8nb97sli2xlag3cdi98mj3137w9i3b";
    };

    parser-combinators = pkgs.fetchFromGitHub {
      owner = "mrkkrp";
      repo = "parser-combinators";
      rev = "dd6599224fe7eb224477ef8e9269602fb6b79fe0";
      sha256 = "11cpfzlb6vl0r5i7vbhp147cfxds248fm5xq8pwxk92d1f5g9pxm";
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

    skipTest = pkgs.haskell.lib.dontCheck;
  in
  {
    dhall = self.callCabal2nix "dhall" "${dhall-haskell}" {};
    megaparsec = self.callCabal2nix "megaparsec" "${megaparsec}" {};
    parser-combinators = self.callCabal2nix "parser-combinators" "${parser-combinators}" {};
    formatting = self.callCabal2nix "formatting" "${formatting}" {};
    prettyprinter = self.callPackage "${dhall-haskell}/nix/prettyprinter.nix" {};

    diagrams-reflex = self.callPackage "${diagrams-reflex}" {};
    reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" "${reflex-dom-contrib}" {};
    semantic-reflex = self.callCabal2nix "semantic-reflex" "${semantic-reflex}/semantic-reflex" {};

    servant = skipTest (self.callCabal2nix "servant" "${servant}/servant" {});
    servant-server = self.callCabal2nix "servant-server" "${servant}/servant-server" {};
    servant-client = self.callCabal2nix "servant-client" "${servant}/servant-client" {};
    servant-client-core = self.callCabal2nix "servant-client-core" "${servant}/servant-client-core" {};
    servant-client-ghcjs = self.callCabal2nix "servant-client-ghcjs" "${servant}/servant-client-ghcjs" {};
    servant-reflex = self.callCabal2nix "servant-reflex" "${servant-reflex}" {};
    servant-auth = self.callCabal2nix "servant-auth" "${servant-auth}/servant-auth" {};
    base-compat = skipTest (self.callCabal2nix "base-compat" "${base-compat}" {});
    aeson = skipTest (self.callCabal2nix "aeson" "${aeson}" {});
    attoparsec = skipTest (self.callCabal2nix "attoparsec" "${attoparsec}" {});
    http-media = skipTest (self.callCabal2nix "http-media" "${http-media}" {});
    # http-types = skipTest (self.callCabal2nix "http-types" "${http-types}" {});
    text = self.callCabal2nix "text" "${text}" {};
    mmorph = self.callCabal2nix "mmorph" "${mmorph}" {};
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
