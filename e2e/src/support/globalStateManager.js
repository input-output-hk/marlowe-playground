class GlobalStateManager {
    constructor() {
        this.state = {};
    }
    appendValue(key, incredmentalValue) {
        this.state[key] = this.state[key] || [];
        this.state[key].push(incredmentalValue);
    }
    popValue(key) {
        if (this.state && this.state[key] && this.state[key].length > 0) {
            return this.state[key].pop();
        }
    }
    getValue(key) {
        const values = this.state[key];
        return values[values.length - 1];
    }
    // method to get value by key
    get(key) {
        return this.state[key];
    }
    // method to set value by key
    set(key, value) {
        this.state[key] = value;
    }
}
export default GlobalStateManager;
