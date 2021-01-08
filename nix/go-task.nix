{ stdenv, gnutar, fetchurl, ... }:

stdenv.mkDerivation rec {
  pname = "go-task";
  version = "3.0.0-preview4";

  targetPath = "$out/bin";

  src = fetchurl {
    url = "https://github.com/go-task/task/releases/download/v3.0.0-preview4/task_linux_amd64.tar.gz";
    sha256 = "0gyds6096k5vzv348zwc16y29231qrc9d7zz9awrsjckknh35zg5";
  };

  unpackPhase = ''
    tar -xzf $src
  '';

  installPhase = ''
    mkdir -p ${targetPath}
    mv ./task ${targetPath}
  '';
}
