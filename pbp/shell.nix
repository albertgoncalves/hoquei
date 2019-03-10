{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "JsonOcaml";
    buildInputs = [ ocaml-ng.ocamlPackages_4_07.ocaml
                    ocaml-ng.ocamlPackages_4_07.yojson
                    ocaml-ng.ocamlPackages_4_07.findlib
                    ocaml-ng.ocamlPackages_4_07.ocp-indent
                    ocaml-ng.ocamlPackages_4_07.utop
                    jq
                    (python36.withPackages(ps: with ps;
                        [ pandas
                          csvkit
                          matplotlib
                        ]
                    ))
                    sqlite
                    rlwrap
                  ];
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            alias ls='ls --color=auto'
            alias ll='ls -al'
        fi
        alias csvlook="csvlook --no-inference -d ';'"
        alias sqlite3="rlwrap sqlite3 -header -csv -separator ';'"
    '';
}
