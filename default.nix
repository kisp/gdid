{
  pkgs ? import <nixpkgs> { },
}:

let
  inherit (pkgs) callPackage;
  gdid = callPackage ./gdid.nix { };
  sbcl' = pkgs.sbcl.withPackages (ps: with ps; [ gdid ]);
  app = pkgs.stdenv.mkDerivation {
    pname = "gdid-app";
    version = "0.0.17";
    src =
      let
        patterns = ''
          *
          !.sbcl-disable-debugger.lisp
        '';
      in
      pkgs.nix-gitignore.gitignoreSourcePure patterns ./.;

    buildInputs = [ sbcl' ];

    dontStrip = true;

    buildPhase = ''
      ${sbcl'}/bin/sbcl --no-userinit --non-interactive \
        --load .sbcl-disable-debugger.lisp \
        --eval '(load (sb-ext:posix-getenv "ASDF"))' \
        --eval '(asdf:load-system :gdid)' \
        --eval '(gdid::dump)'
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp gdid $out/bin
    '';
  };
in
{
  inherit app;
  lib = gdid;
}
