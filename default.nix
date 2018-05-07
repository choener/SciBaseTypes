with import <nixpkgs> {};
haskellPackages.extend (haskell.lib.packageSourceOverrides {
  SciBaseTypes = ./.;
})

