import * as fs from 'fs';
export const env = (key) => {
    const value = process.env[key];
    if (!value) {
        throw Error(`No environment variable found for ${key}`);
    }
    return value;
};
export const getJsonFromFile = (path) => {
    const buff = fs.readFileSync(`${process.cwd()}${path}`);
    const json = buff.toString();
    return JSON.parse(json);
};
export const envNumber = (key) => {
    return Number(env[key]);
};
