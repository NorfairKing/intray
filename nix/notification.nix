{ writeShellScript
, intrayReleasePackages
}:
{ userName
, accessKey
, intrayUrl ? "https://api.intray.cs-syd.eu"
}:

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
  export INTRAY_PASSWORD="${accessKey}"
  ${cli}/bin/intray login
  ${cli}/bin/intray add --stdin --remote "$@"
  rm -rf $tempDir
''
