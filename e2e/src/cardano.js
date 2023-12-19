// TODO: Implement Bech32 encoding/decoding from cbor and from string
export class Bech32 {
    constructor(s) {
        this.value = s.trim();
    }
    static fromString(s) {
        //TODO: validatate the value
        return new Bech32(s);
    }
    toString() {
        return this.value;
    }
}
export class ContractId {
    constructor(s) {
        this.value = s.trim();
    }
    static fromString(s) {
        // TODO: validate the value
        return new ContractId(s);
    }
    toString() {
        return this.value;
    }
}
