with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = {SciBaseTypes = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.SciBaseTypes ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
