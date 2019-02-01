self:

let
  tgz = builtins.fetchTarball {
    url = "http://hackage.haskell.org/package/semirings-0.3.1.1/semirings-0.3.1.1.tar.gz";
    sha256 = "1wi4g4xk3vjqig2mrgdc09ygwcdirlpky00xikak8cndkydcm2za";
  };
in

# NOTE: NO callCabal2nix because this needs the *source* override only!

{ # semirings = "0.2.1.1";
  semirings = tgz;
}
