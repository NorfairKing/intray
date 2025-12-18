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
          options = import ../intray-cli/options.nix { inherit lib; };
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


      cli = cfg.intray-cli;

      syncIntrayName = "sync-intray";
      syncIntrayService = opt-env-conf.addSettingsCheckToUserService { read-secret = false; } {
        Unit = {
          Description = "Sync intray items";
          Wants = [ "network-online.target" ];
        };
        Service = {
          ExecStartPre = "${cli}/bin/intray login";
          ExecStart = "${cli}/bin/intray sync";
          Environment = [ "INTRAY_CONFIG_FILE=${intrayConfigFile}" ];
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
      systemd.user = {
        services = services;
        timers = timers;
      };
      home.packages = [ cli ];
    };
}
