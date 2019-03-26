{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "R";
    buildInputs = [
        glibcLocales
        (with rPackages; [
            R
            # lintr
        ])
        gawk
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

        d="data/"
        if [ ! -d $d ]; then
            mkdir $d
            echo "Need to snag some data for this to work!"
        fi

        stan="cmdstan/"

        if [ ! -d $stan ]; then
            git clone https://github.com/stan-dev/cmdstan.git --recursive
            cd $stan
            make build
        else
            cd $stan
            make stan-update
        fi

        cd ../

        lintr() {
            R -e "library(lintr); lint('$1')" \
                | awk '/> /{ found=1 } { if (found) print }'
        }

        export -f lintr
    '';
}
