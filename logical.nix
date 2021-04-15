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

      services.nginx.virtualHosts."www.parasrah.com" = {
        locations."/".return = "301 https://parasrah.com$request_uri";
      };
    };
}
