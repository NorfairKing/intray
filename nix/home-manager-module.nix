{ intray-cli
, opt-env-conf
}:
{ lib
, pkgs
, config
, ...
}:

with lib;

let
  cfg = config.programs.intray;

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

in
{
  options = {
    programs.intray = {
      enable = mkEnableOption "Intray cli";
      intray-cli = mkOption {
        description = "The intray-cli attribute defined in the nix/overlay.nix file in the intray repository.";
        default = intray-cli;
      };
      config = mkOption {
        default = { };
        type = types.submodule {
          options = (pkgs.callPackage ../intray-cli/options.nix { });
        };
      };
      extraConfig = mkOption {
        description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
        default = { };
      };
      sync.enable = mkEnableOption "Automatic intray synchronisation";
    };
  };
  config =
    let
      intrayConfig = mergeListRecursively [
        (builtins.removeAttrs cfg.config [ "override" "overrideDerivation" ])
        cfg.extraConfig
      ];
      intrayConfigFile = (pkgs.formats.yaml { }).generate "intray-config.yaml" intrayConfig;

      settingsCheck = opt-env-conf.makeSettingsCheck "intray-settings-check"
        "${cli}/bin/intray"
        [ "--config-file" intrayConfigFile "sync" ]
        { };

      cli = cfg.intray-cli;

      syncIntrayName = "sync-intray";
      syncIntrayService = {
        Unit = {
          Description = "Sync intray items";
          Wants = [ "network-online.target" ];
        };
        Service = {
          ExecStart = "${pkgs.writeShellScript "intray-sync" ''
              ${cli}/bin/intray login
              ${cli}/bin/intray sync
            ''}";
          Type = "oneshot";
        };
      };

      syncIntrayTimer = {
        Unit = {
          Description = "Sync intray items every five minutes";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
        Timer = {
          OnCalendar = "*:0/5";
          Persistent = true;
          Unit = "${syncIntrayName}.service";
        };
      };
      services = optionalAttrs cfg.sync.enable {
        "${syncIntrayName}" = syncIntrayService;
      };
      timers = optionalAttrs cfg.sync.enable {
        "${syncIntrayName}" = syncIntrayTimer;
      };
    in
    mkIf cfg.enable {
      xdg.configFile."intray/config.yaml".source = "${intrayConfigFile}";
      xdg.configFile."intray/settings-check.txt".source = "${settingsCheck}";
      systemd.user = {
        services = services;
        timers = timers;
      };
      home.packages = [ cli ];
    };
}
