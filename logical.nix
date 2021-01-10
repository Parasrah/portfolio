{
  kali = { config, pkgs, lib, ... }:
    let
      portfolio =
        pkgs.callPackage ./default.nix { env = "prod"; };
    in
    {
      networking = {
        firewall = {
          enable = true;
          allowPing = true;
          allowedTCPPorts = [ 22 80 443 ];
          allowedUDPPorts = [ ];
        };
      };

      services = {
        nginx = {
          enable = true;
          recommendedGzipSettings = true;
          recommendedOptimisation = true;
          recommendedTlsSettings = true;
          virtualHosts."parasrah.com" = {
            forceSSL = true;
            root = "${portfolio}/www";
            sslCertificate = ./.secrets/ssl/parasrah_com.crt;
            sslCertificateKey = ./.secrets/ssl/parasrah.key;
          };
        };
      };
    };
}
