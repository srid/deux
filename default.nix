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
      rev = "9211a8ee9b9e97d36f78225da31ed1f885f467aa";
      sha256 = "102asj8jadz2cajghd8j88dmd7c2frwial18j9iflirbs8vs5x64";
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
  in
  {
    diagrams-reflex = self.callPackage "${diagrams-reflex}" {};
    reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" "${reflex-dom-contrib}" {};
    semantic-reflex = self.callCabal2nix "semantic-reflex" "${semantic-reflex}/semantic-reflex" {};
    servant-reflex = self.callCabal2nix "servant-reflex" "${servant-reflex}" {};
    servant-auth = self.callCabal2nix "servant-auth" "${servant-auth}/servant-auth" {};
    Glob = self.callCabal2nix "Glob" "${glob}" {};
  };
})
