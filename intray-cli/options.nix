{ lib }:
{
  auto-open = lib.mkOption {
    default = null;
    description = "How to auto-open";
    type = lib.types.nullOr (lib.types.oneOf [
      lib.types.bool
      lib.types.str
    ]);
  };
  cache-dir = lib.mkOption {
    default = null;
    description = "directory to store cache information. You can remove this directory as necessary.";
    type = lib.types.nullOr lib.types.str;
  };
  data-dir = lib.mkOption {
    default = null;
    description = "directory to store data information. Removing this directory could lead to data loss.";
    type = lib.types.nullOr lib.types.str;
  };
  log-level = lib.mkOption {
    default = null;
    description = "minimal severity for log message";
    type = lib.types.nullOr (lib.types.enum [
      "Debug"
      "Info"
      "Warn"
      "Error"
    ]);
  };
  password = lib.mkOption {
    default = null;
    description = "Password";
    type = lib.types.nullOr lib.types.str;
  };
  password-file = lib.mkOption {
    default = null;
    description = "Password file";
    type = lib.types.nullOr lib.types.str;
  };
  sync-strategy = lib.mkOption {
    default = null;
    description = "sync strategy for non-sync commands.";
    type = lib.types.nullOr (lib.types.enum [
      "NeverSync"
      "AlwaysSync"
    ]);
  };
  url = lib.mkOption {
    default = null;
    description = "api url of the intray server.";
    type = lib.types.nullOr lib.types.str;
  };
  username = lib.mkOption {
    default = null;
    description = "Username";
    type = lib.types.nullOr lib.types.str;
  };
}
