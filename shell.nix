{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Selenium";
    buildInputs = [ python36
                    python36Packages.selenium
                    python36Packages.csvkit
                    fzf
                  ];
    shellHook = ''
        export chromedriver_zip="chromedriver_linux64.zip"
        export chromedriver_path="./chromedriver"

        if [ ! -e $chromedriver_zip ]; then
            wget "https://chromedriver.storage.googleapis.com/2.45/$chromedriver_zip"
        fi

        if [ ! -e $chromedriver_path ]; then
            unzip $chromedriver_zip -d ./
        fi

        strcd() { cd "$(dirname $1)"; }
        withfzf() {
            local h
            h=$(fzf)
            if (( $? == 0 )); then
                $1 "$h"
            fi
        }

        alias cdfzf="withfzf strcd"
        alias vimfzf="withfzf vim"

        export -f withfzf
    '';
}
