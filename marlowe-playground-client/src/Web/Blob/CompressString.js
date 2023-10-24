import LZString from 'lz-string';

export function compressToURI(originalString) {
    return LZString.compressToEncodedURIComponent(originalString);
}

export function decompressFromURI(compressedString) {
    return LZString.decompressFromEncodedURIComponent(compressedString);
}
