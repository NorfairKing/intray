{ pkgs
, home-manager
, intray-nixos-module-factory
, intray-home-manager-module
}:
let
  intray-production = intray-nixos-module-factory {
    envname = "production";
  };

  api-port = 8000;
  web-port = 8080;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "intray-module-test";
    nodes = {
      apiserver = {
        imports = [
          intray-production
        ];
        services.intray.production = {
          enable = true;
          api-server = {
            enable = true;
            port = api-port;
            log-level = "Debug";
          };
        };
      };
      webserver = {
        imports = [
          intray-production
        ];
        services.intray.production = {
          enable = true;
          web-server = {
            enable = true;
            port = web-port;
            api-url = "apiserver:${builtins.toString api-port}";
          };
        };
      };
      client = {
        imports = [
          home-manager
        ];
        users.users.testuser.isNormalUser = true;
        home-manager = {
          useGlobalPkgs = true;
          users.testuser = { pkgs, ... }: {
            imports = [
              intray-home-manager-module
            ];
            xdg.enable = true;
            programs.intray = {
              enable = true;
              intrayReleasePackages = pkgs.intrayReleasePackages;
              sync = {
                enable = true;
                url = "http://apiserver:${builtins.toString api-port}";
                username = "testuser";
                password = "testpassword";
              };
            };
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      apiserver.start()
      webserver.start()
      client.start()

      apiserver.wait_for_unit("multi-user.target")
      webserver.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")

      apiserver.wait_for_unit("intray-api-server-production.service")
      webserver.wait_for_unit("intray-web-server-production.service")

      apiserver.wait_for_open_port(${builtins.toString api-port})
      client.succeed("curl apiserver:${builtins.toString api-port}")

      webserver.wait_for_open_port(${builtins.toString web-port})
      client.succeed("curl webserver:${builtins.toString web-port}")

      client.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      out = client.succeed(su("testuser", "cat ~/.config/intray/config.yaml"))
      print(out)

      # client.succeed(su("testuser", "intray register"))
      # client.succeed(su("testuser", "intray login"))
      # client.succeed(su("testuser", "intray sync"))
    '';
  }
)
