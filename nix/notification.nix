{ lib
, writeShellScript
, intrayReleasePackages
}:
{ userName
, accessKey ? null
, accessKeyFile ? null
, intrayUrl ? "https://api.intray.cs-syd.eu"
}:
with lib;
assert (!(builtins.isNull accessKey && builtins.isNull accessKeyFile)); # "Either accessKey or accessKeyFile must be set.
let
  cli = intrayReleasePackages.intray-cli;
in
writeShellScript "intray-notification" ''
  set -eou pipefail
  tempDir="$(mktemp --tmpdir=/tmp --directory intray-notification-XXXXXXXX)"
  export INTRAY_CACHE_DIR="$tempDir"
  export INTRAY_DATA_DIR="$tempDir"
  export INTRAY_CONFIG_FILE="$tempDir/config.yaml"
  export INTRAY_URL="${intrayUrl}"
  export INTRAY_USERNAME="${userName}"
  export INTRAY_SYNC_STRATEGY="NeverSync"
  ${optionalString (!builtins.isNull accessKey) ("export INTRAY_PASSWORD=${accessKey}")}
  ${optionalString (!builtins.isNull accessKeyFile) ("export INTRAY_PASSWORD_FILE=${accessKeyFile}")}
  ${cli}/bin/intray login
  ${cli}/bin/intray add --stdin --remote "$@"
  rm -rf $tempDir
''
