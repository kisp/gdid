{
  pkgs ? import <nixpkgs> { },
}:
let
  inherit (pkgs) sbcl callPackage;
  trivial-template = callPackage ./trivial-template.nix { };
  hgetopt = callPackage ./hgetopt.nix { };
  inherit
    (sbcl.withPackages (
      ps: with ps; [
        alexandria
        hgetopt
        cl-pdf
        cl-typesetting
        cl-ppcre
      ]
    ))
    lispLibs
    ;
in
sbcl.buildASDFSystem {
  pname = "gdid";
  version = "0.0.16";
  src =
    let
      patterns = ''
        *
        !gdid.asd
        !version.lisp-expr
        !packages.lisp
        !model.lisp
        !utils.lisp
        !pdf.lisp
        !commands.lisp
        !gdid.lisp
      '';
    in
    pkgs.nix-gitignore.gitignoreSourcePure patterns ./.;

  systems = [ "gdid" ];

  lispLibs = lispLibs;
}
