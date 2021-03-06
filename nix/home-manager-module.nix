{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.intray;

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

  toYamlFile = pkgs.callPackage ./to-yaml.nix { };

in
{
  options =
    {
      programs.intray =
        {
          enable = mkEnableOption "Intray cli";
          intrayPackages = mkOption {
            description = "The intrayPackages attribute defined in the nix/overlay.nix file in the intray repository.";
            default = (import ./pkgs.nix { }).intrayPackages;
          };
          config = mkOption {
            default = { };
            description = "The contents of the intray cli config file";
          };
          cache-dir = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "The cache dir";
          };
          data-dir = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "The data dir";
          };
          sync = mkOption {
            default = null;
            type =
              types.nullOr (
                types.submodule {
                  options = {
                    enable = mkEnableOption "Intray synchronisation";
                    username = mkOption {
                      type = types.str;
                      example = "syd";
                      description = "The username to use for syncing";
                    };
                    password = mkOption {
                      type = types.str;
                      description = "The password to use for syncing";
                    };
                    url = mkOption {
                      type = types.str;
                      default = "https://api.intray.eu";
                      description =
                        "The sync server to use for syncing";
                    };
                  };
                }
              );
          };
        };
    };
  config =
    let
      nullOrOption =
        name: opt: optionalAttrs (!builtins.isNull opt) { "${name}" = opt; };
      syncConfig = optionalAttrs (cfg.sync.enable or false) {
        url = cfg.sync.url;
        username = cfg.sync.username;
        password = cfg.sync.password;
        sync = "NeverSync";
      };

      commonConfig = mergeListRecursively [
        (nullOrOption "cache-dir" cfg.cache-dir)
        (nullOrOption "data-dir" cfg.data-dir)
      ];
      intrayConfig = mergeListRecursively [
        syncConfig
        commonConfig
        cfg.config
      ];
      intrayConfigFile = toYamlFile "intray-config" intrayConfig;
      cli = cfg.intrayPackages.intray-cli;

      syncIntrayName = "sync-intray";
      syncIntrayService =
        {
          Unit =
            {
              Description = "Sync intray items";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "intray-sync" ''
                  ${cli}/bin/intray login
                  ${cli}/bin/intray sync
                ''}";
              Type = "oneshot";
            };
        };

      syncIntrayTimer =
        {
          Unit =
            {
              Description = "Sync intray items every five minutes";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "*:0/5";
              Persistent = true;
              Unit = "${syncIntrayName}.service";
            };
        };
      services =
        optionalAttrs (cfg.sync != null && cfg.sync.enable) {
          "${syncIntrayName}" = syncIntrayService;
        };
      timers =
        optionalAttrs (cfg.sync != null && cfg.sync.enable) {
          "${syncIntrayName}" = syncIntrayTimer;
        };
    in
    mkIf cfg.enable {
      xdg.configFile."intray/config.yaml".source = "${intrayConfigFile}";
      systemd.user =
        {
          services = services;
          timers = timers;
        };
      home.packages = [ cli ];
    };
}
