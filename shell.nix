{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Python";
    buildInputs = [
        (python37.withPackages(ps: with ps; [
            selenium
            matplotlib
            numpy
            flake8
            pandas
            # csvkit
        ]))
        fzf
        wget
    ] ++ (with python36Packages; [
        (csvkit.overridePythonAttrs (oldAttrs: {checkPhase = "true";}))
    ]);
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            os="mac"
            alias ls='ls --color=auto'
            alias ll='ls -al'
        else
            os="linux"
        fi

        chromedriver_zip=chromedriver_"$os"64.zip
        chromedriver_path="chromedriver"

        if [ ! -e $chromedriver_zip ]; then
            wget "https://chromedriver.storage.googleapis.com/2.45/$chromedriver_zip"
        fi

        if [ ! -e $chromedriver_path ]; then
            unzip $chromedriver_zip -d ./
        fi

        for d in data viz; do
            if [ ! -e ./$d/ ]; then
                mkdir $d
            fi
        done

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
        alias csvlook="csvlook --no-inference"

        export -f withfzf
        export chromedriver_path=$(pwd)/$chromedriver_path
    '';
}
