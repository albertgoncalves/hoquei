{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Selenium";
    buildInputs = [ python36
                    python36Packages.selenium
                    python36Packages.csvkit
                    python36Packages.pandas
                    python36Packages.matplotlib
                    python36Packages.numpy
                    python36Packages.flake8
                    fzf
                    wget
                  ];
    shellHook = ''
        alias ls='ls -GFh'
        alias ll='ls -al'

        chromedriver_zip="chromedriver_mac64.zip"
        chromedriver_path="chromedriver"

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
        alias flake8="flake8 --ignore E124,E128,E201,E203,E241,W503"

        export -f withfzf
        export chromedriver_path=$(pwd)/$chromedriver_path
    '';
}