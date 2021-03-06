{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "JsonOcaml";
    buildInputs = [
        (with ocaml-ng.ocamlPackages_4_07; [
            ocaml
            yojson
            findlib
            ocp-indent
            utop
        ])
        (python37.withPackages(ps: with ps; [
            pandas
            matplotlib
            flake8
        ]))
        jq
        sqlite
        rlwrap
    ] ++ (with python36Packages; [
        (csvkit.overridePythonAttrs (oldAttrs: {checkPhase = "true";}))
    ]);
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
