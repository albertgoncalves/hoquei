{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Haskell";
    buildInputs = [
        (haskell.packages.ghc864.ghcWithPackages (pkgs: [
            pkgs.hlint
            pkgs.hoogle
        ]))
        libiconv
    ] ++ (with python36Packages; [
        (csvkit.overridePythonAttrs (oldAttrs: {checkPhase = "true";}))
    ]);
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            alias ls='ls --color=auto'
            alias ll='ls -al'
        fi
        alias hlint="hlint -c=never"
        alias csvlook="csvlook --no-inference"
    '';
}
