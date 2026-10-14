{
  lib,
  stdenv,
  runCommand,
  makeWrapper,
  haskell,
  faust,
  sox,
  which,
  pipewire,
  fau-unwrapped,
  toolchain,
}:

let
  runtimeInputs = [
    faust
    stdenv.cc
    sox
    which
  ]
  ++ lib.optional stdenv.hostPlatform.isLinux pipewire; # for pw-play
in
runCommand "fau"
  {
    nativeBuildInputs = [ makeWrapper ];
    meta.mainProgram = "fau";
  }
  ''
    makeWrapper ${haskell.lib.justStaticExecutables fau-unwrapped}/bin/fau $out/bin/fau \
      --prefix PATH : ${lib.makeBinPath runtimeInputs} \
      ${lib.concatStringsSep " " (
        lib.mapAttrsToList (name: value: "--set ${name} ${lib.escapeShellArg value}") toolchain
      )}
  ''
