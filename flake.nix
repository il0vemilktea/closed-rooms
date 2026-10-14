{
  description = "fau-noise: Faust textures rendered to wav";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        python-env = pkgs.python3.withPackages (ps:
          with ps; [
            ipython
            jupyter
            tqdm
            numpy
            numba
            pyarrow
            pandas
            polars
            scipy
            scikit-learn
            librosa
            statsmodels
            matplotlib
            seaborn
          ]);

        haskellPackages = pkgs.haskellPackages.extend
          (hself: _: { fau = hself.callCabal2nix "fau" ./fau { }; });

        fau = pkgs.callPackage ./nix/fau.nix {
          fau-unwrapped = haskellPackages.fau;
          inherit toolchain;
        };

        toolchain = {
          FAU_FAUST_INC = "${pkgs.faust}/include";
          FAU_SNDFILE_INC = "${pkgs.lib.getDev pkgs.libsndfile}/include";
          FAU_SNDFILE_LIB = "${pkgs.lib.getLib pkgs.libsndfile}/lib";
          FAU_BUILD_KEY = "${pkgs.faust} ${pkgs.stdenv.cc} ${pkgs.libsndfile}";
        };
      in {
        packages = {
          inherit fau;
          default = fau;

          # render all faust dsps with default params
          renders =
            pkgs.runCommand "fau-renders" { nativeBuildInputs = [ fau ]; } ''
              cp -r ${self} root && chmod -R u+w root && cd root
              export FAU_ROOT=$PWD
              mkdir -p $out
              for f in src/*.dsp; do
                fau render "$f" -o "$out/$(basename "$f" .dsp).wav"
              done
            '';
        };

        devShells.default = haskellPackages.shellFor (toolchain // {
          packages = hp: [ hp.fau ];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.haskell-language-server
            pkgs.fourmolu

            fau
            pkgs.faust
            pkgs.sox

            python-env
          ];

          shellHook = ''
            export FAUST_LIB_PATH="$PWD/lib"
          '';
        });
      });
}
