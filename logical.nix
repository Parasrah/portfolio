{
  kali = { config, pkgs, lib, ... }:
    let
      portfolio =
        pkgs.callPackage ./default.nix { env = "prod"; };
    in
    {
      services.nginx.virtualHosts."parasrah.com" = {
        forceSSL = true;
        root = "${portfolio}/www";
        sslCertificate = ./.secrets/ssl/parasrah_com.crt;
        sslCertificateKey = ./.secrets/ssl/parasrah.key;
      };
    };
}
