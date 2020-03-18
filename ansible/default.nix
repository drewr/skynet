with import <nixpkgs> {};
with pkgs.python37Packages;

stdenv.mkDerivation {
  name = "ansible";
  buildInputs = [
    (ansible_2_9.override { windowsSupport = true; })
    awscli
    boto3
    jq
    vault
  ];
}
