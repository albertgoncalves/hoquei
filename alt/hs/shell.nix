{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Haskell";
    buildInputs = [
        (haskell.packages.ghc864.ghcWithPackages (pkgs: [
            hlint
        ]))
        libiconv
        haskellPackages.hindent
    ];
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            alias ls='ls --color=auto'
            alias ll='ls -al'
        fi
        alias hlint="hlint -c=never"
        alias hindent="hindent --indent-size 4 --sort-imports --line-length 79"
    '';
}
