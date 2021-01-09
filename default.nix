{ env, pkgs }:

let
  name = "portfolio";

  inherit (pkgs) mkYarnPackage;

  inherit (pkgs.elmPackages) fetchElmDeps;

  yarnPkg = mkYarnPackage {
    name = "${name}-packages";
    packageJSON = ./package.json;
    src = ./.;
    patchPhase = ":";
    yarnLock = ./yarn.lock;
    publishBinsFor = [ "webpack" ];
  };

in
  with pkgs; stdenv.mkDerivation {
    WEBPACK_ENV = env;
    NODE_ENV = if env == "prod" then "production" else "development";
    LOCALE_ARCHIVE_2_27 = "${glibcLocales}/lib/locale/locale-archive";
    LOCALE_ARCHIVE_2_11 = "${glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";

    inherit name;

    src = nix-gitignore.gitignoreSource [] ./.;

    doCheck = true;

    buildInputs = with elmPackages; [
      elm
      yarn
      yarnPkg
      elm2nix
      elm-test
      elm-format
      elm-analyse
      nodejs-14_x
      glibcLocales
    ];

    patchPhase = ''
      rm -rf elm-stuff
      ln -sf ${yarnPkg}/libexec/${name}/node_modules .
    '';

    buildPhase = ''
      WEBPACK_ENV=prod webpack
    '';

    checkPhase = ''
      elm-format src/ --validate
      elm-test
    '';

    installPhase = ''
      mkdir -p $out
      mv ./dist $out/www
    '';

    configurePhase = fetchElmDeps {
      elmVersion = "0.19.1";
      elmPackages = import ./elm-srcs.nix;
      registryDat = ./registry.dat;
    };
  }
