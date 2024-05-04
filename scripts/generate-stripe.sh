nix build .#generatedIntrayStripeCode
rm -rf intray-stripe-client
mkdir -p intray-stripe-client
cp -R result/* intray-stripe-client
chmod -R 764 intray-stripe-client
