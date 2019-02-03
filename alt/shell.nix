{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "OCaml";
    buildInputs = [ ocaml-ng.ocamlPackages_4_07.ocaml
                    ocaml-ng.ocamlPackages_4_07.ocaml_extlib
                    ocaml-ng.ocamlPackages_4_07.findlib
                    ocaml-ng.ocamlPackages_4_07.ocp-indent
                    ocaml-ng.ocamlPackages_4_07.utop
                  ];
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            alias ls='ls --color=auto'
            alias ll='ls -al'
        fi
    '';
}
