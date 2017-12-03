with import <nixpkgs> {};
with pkgs.python27Packages;

stdenv.mkDerivation {
  name = "ansible";
  buildInputs = [
    (ansible_2_3.override { windowsSupport = true; })
    awscli
    boto3
    jq
    vault
  ];
}
