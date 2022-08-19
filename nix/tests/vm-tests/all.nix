{ makeTest
, lib
, docs
, marlowe-playground
, web-ghc
, sources
, vmCompileTests # when enabled the test tries to compile plutus/marlowe code on webghc
}:
let
  marloweApiRequest = builtins.toFile "marlowe-request.json" (builtins.readFile ./runghc-api-request.json);
in
makeTest {
  skipLint = true;
  name = "all";
  nodes = {
    # ---------------------------------------------------------------------------------------------------------------
    # playgrounds : 192.168.1.2 - running plutus/marlowe playgrounds and nginx
    # --------------------------------------------------------------------------------------------------------------

    playgrounds = { pkgs, ... }: {
      imports = [
        ../../modules/marlowe-playground.nix
      ];

      networking = {
        firewall.allowedTCPPorts = [ 7070 9090 ];
        extraHosts = ''
          127.0.0.1 marlowe-playground
          192.168.1.3 webghc
        '';
        dhcpcd.enable = false;
        interfaces.eth1.ipv6.addresses = lib.mkOverride 0 [{ address = "fd00::2"; prefixLength = 64; }];
        interfaces.eth1.ipv4.addresses = lib.mkOverride 0 [{ address = "192.168.1.2"; prefixLength = 24; }];
      };

      services = {
        marlowe-playground = {
          enable = true;
          port = 4001;
          playground-server-package = marlowe-playground.server;
        };

        nginx = {
          enable = true;
          recommendedGzipSettings = true;
          recommendedProxySettings = true;
          recommendedOptimisation = true;

          upstreams = {
            marlowe-playground.servers."127.0.0.1:4001" = { };
          };
          virtualHosts = {
            "marlowe-playground" = {
              listen = [{ addr = "0.0.0.0"; port = 9090; }];
              locations = {
                "/" = {
                  root = "${marlowe-playground.client}";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "^~ /doc/" = {
                  alias = "${docs.site}/";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "/runghc" = {
                  proxyPass = "http://webghc";
                };
                "@fallback" = {
                  proxyPass = "http://marlowe-playground";
                  proxyWebsockets = true;
                };
              };
            };
          };
        };
      };

      environment.systemPackages = with pkgs; [ curl ];
    };

    # ---------------------------------------------------------------------------------------------------------------
    # webghc : 192.168.1.3 - running webghc with plutus/marlowe deps
    # --------------------------------------------------------------------------------------------------------------

    webghc = { pkgs, ... }: {

      virtualisation.memorySize = "1024";

      networking = {
        firewall.allowedTCPPorts = [ 80 ];
        dhcpcd.enable = false;
        interfaces.eth1.ipv6.addresses = lib.mkOverride 0 [{ address = "fd00::3"; prefixLength = 64; }];
        interfaces.eth1.ipv4.addresses = lib.mkOverride 0 [{ address = "192.168.1.3"; prefixLength = 24; }];
      };

      imports = [
        (sources.plutus-apps + "/nix/modules/web-ghc.nix")
      ];
      services = {
        web-ghc = {
          enable = true;
          port = 80;
          web-ghc-package = web-ghc;
        };
      };
    };
  };
  testScript = ''
    playgrounds.start()
    webghc.start()

    #
    # assert connectivity
    #
    playgrounds.wait_for_unit("network-online.target")

    webghc.wait_for_unit("network-online.target")
    playgrounds.succeed("ping -c1 192.168.1.1")
    playgrounds.succeed("ping -c1 192.168.1.2")
    playgrounds.succeed("ping -c1 192.168.1.3")
    webghc.succeed("ping -c1 192.168.1.1")
    webghc.succeed("ping -c1 192.168.1.2")
    webghc.succeed("ping -c1 192.168.1.3")


    #
    # playground / frontend asserts
    #
    playgrounds.wait_for_unit("marlowe-playground.service")
    playgrounds.wait_for_unit("nginx.service")
    playgrounds.wait_for_open_port(7070)
    playgrounds.wait_for_open_port(9090)

    with subtest("********************************************************************************************* TEST: All content is being served on playgrounds"):
      res = playgrounds.succeed("curl --silent http://marlowe-playground:9090/")
      assert "marlowe-playground" in res, "Expected string 'marlowe-playground' from 'http://marlowe-playground:9090'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://marlowe-playground:9090/doc/")
      assert "marlowe-playground" in res, "Expected string 'The Plutus Platform' from 'http://marlowe-playground:9090/doc'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://marlowe-playground:9090/doc/marlowe/tutorials/")
      assert "Tutorials" in res, "Expected string 'Tutorials' from 'http://marlowe-playground:9090/doc/marlowe/tutorials'. Actual: {}".format(res)

    #
    # webghc asserts
    #
    webghc.wait_for_unit("web-ghc.service")
    webghc.wait_for_open_port(80)

  '' + lib.optionalString (vmCompileTests) ''
    #
    # plutus-playground / webghc : using api/contract
    # marlowe-playground / webghc : using /runghc
    #
    with subtest("********************************************************************************************* TEST: compilation works"):
      res = playgrounds.succeed("curl --silent -H 'Content-Type: application/json' --request POST --data @${marloweApiRequest} http://marlowe-playground:9090/runghc")
      assert "Right" in res, "Expected response wrapped in 'Right'. Actual: {}".format(res)
  '';
}
