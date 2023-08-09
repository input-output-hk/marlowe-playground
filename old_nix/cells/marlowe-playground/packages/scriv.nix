{ inputs, cell }:
let
  inherit (cell.library) pkgs;
  inherit (pkgs) python3;
  inherit (python3.pkgs) buildPythonPackage fetchPypi attrs click click-log requests jinja2;
in
buildPythonPackage rec {
  pname = "scriv";

  version = "0.17.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-jyOIPvg9/FDwn3au8I/zBz8nUsclXbFdJM2L/swyN5w=";
  };

  propagatedBuildInputs = [
    attrs
    click
    click-log
    jinja2
    requests
  ];

  doCheck = false;

  meta = {
    homepage = "https://github.com/nedbat/scriv";
    description = "";
  };
}
