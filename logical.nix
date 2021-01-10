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
          # recommendedTlsSettings = true;
          virtualHosts."parasrah.com" = {
            # onlySSL = true;
            # enableACME = true;
            root = "${portfolio}/www";
          };
        };
      };

      # security.acme = {
      #   acceptTerms = true;
      #   certs = {
      #     "parasrah.com".email = "portfolio@parasrah.com";
      #   };
      # };
    };
}
