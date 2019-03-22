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
                          flake8
                        ]
                    ))
                    sqlite
                    rlwrap
                  ];
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            alias ls='ls --color=auto'
            alias ll='ls -al'
        else
            alias open="xdg-open"
        fi
        alias csvlook="csvlook --no-inference -d ';'"
        alias sqlite3="rlwrap sqlite3 -header -csv -separator ';'"
        alias flake8="flake8 --ignore E124,E128,E201,E203,E241,E402,W503"
    '';
}
