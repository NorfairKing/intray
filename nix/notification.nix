{ lib
, writeShellScript
, intray-notification
}:
{ userName
, accessKey ? null
, accessKeyFile ? null
, intrayUrl ? "https://api.intray.cs-syd.eu"
}:
with lib;
assert (!(builtins.isNull accessKey && builtins.isNull accessKeyFile)); # "Either accessKey or accessKeyFile must be set.
writeShellScript "intray-notification" ''
  set -eou pipefail
  exec ${intray-notification}/bin/intray-notification \
    "--url=${intrayUrl}" \
    "--username=${userName}" \
    ${optionalString (!builtins.isNull accessKey) ("--key=${accessKey}")} \
    ${optionalString (!builtins.isNull accessKeyFile) ("--key-file=${accessKeyFile}")} \
    "$@"
''
