{ inputs, cell }:

let
  inherit (cell.library) write-formatter purescript;
  inherit (purescript) purs-tidy;

in

write-formatter "purs-tidy" "${purs-tidy}/bin/purs-tidy format-in-place" [ "purs" ]
