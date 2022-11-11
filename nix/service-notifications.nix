{ intrayRelease }:
{ lib, config, ... }:
with lib;

let
  cfg = config.services.intray.notifications;
in
{
  options.services.intray.notifications =
    {
      enable = mkEnableOption "Intray service notifications";
      services =
        mkOption {
          type = types.listOf (types.string);
          example = [ "intray-production" ];
          description = "The services to notify on failure for";
        };
      userName =
        mkOption {
          type = types.str;
          example = "syd";
          description = "The username to log into the intray service";
        };
      accessKey =
        mkOption {
          type = types.str;
          example = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
          description = "The access key to log into the intray service";
        };
      intrayUrl =
        mkOption {
          type = types.str;
          default = "https://api.intray.cs-syd.eu";
          example = "https://api.intray.cs-syd.eu";
          description = "The access key to log into the intray service";
        };
    };
  config =
    {
      systemd.services =
        let
          notificationScript =
            intrayRelease.notification {
              inherit (cfg) userName accessKey intrayUrl;
            };

          notifyScript =
            serviceName:
            # This must be a single line.
            ''
              %s -c '[[ "$SERVICE_RESULT" == "success" ]] || ${notificationScript} "${config.networking.hostName}: Systemd user service ${serviceName}.service exited with exit code $EXIT_STATUS"'
            '';

        in
        genAttrs cfg.services (
          serviceName:
          { serviceConfig.ExecStopPost = notifyScript serviceName; }
        );
    };
}
