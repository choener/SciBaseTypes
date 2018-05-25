with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    SciBaseTypes = ./.;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.SciBaseTypes ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
    ];
  };
}
