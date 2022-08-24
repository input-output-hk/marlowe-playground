import { TarReader, TarWriter } from "tarballjs";

export function tarWriter() {
  return new TarWriter();
}

export function tarReader() {
  return new TarReader();
}
