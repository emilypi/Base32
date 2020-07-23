{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, criterion, deepseq
      , ghc-byteorder, memory, QuickCheck, random-bytestring, stdenv
      , tasty, tasty-hunit, tasty-quickcheck, text, text-short
      }:
      mkDerivation {
        pname = "base32";
        version = "0.2.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring deepseq ghc-byteorder text text-short
        ];
        testHaskellDepends = [
          base bytestring memory QuickCheck random-bytestring tasty
          tasty-hunit tasty-quickcheck text text-short
        ];
        benchmarkHaskellDepends = [
          base bytestring criterion deepseq memory random-bytestring text
        ];
        homepage = "https://github.com/emilypi/base32";
        description = "Fast RFC 4648-compliant Base32 encoding";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
