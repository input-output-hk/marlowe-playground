// The purpose of this file is to be able to enable dynamic paramters to be passed via the actual cucumber arguments 
// that we pass when calling the cucumber executable.
import dotenv from 'dotenv';
import { env, getJsonFromFile } from './env/parseEnv.js';
import * as fs from 'fs';
dotenv.config({ path: './env/common.env' });
const hostsConfig = getJsonFromFile(env('HOSTS_URLS_PATH'));
const pagesConfig = getJsonFromFile(env('PAGE_URLS_PATH'));
const pageMappingFiles = fs.readdirSync(`${process.cwd()}${env('PAGE_ELEMENT_MAPPINGS_PATH')}`);
const pageElementMappings = pageMappingFiles.reduce((pageElementConfigAcc, file) => {
    const key = file.replace('.json', '');
    const elementMappings = getJsonFromFile(`${env('PAGE_ELEMENT_MAPPINGS_PATH')}${file}`);
    return { ...pageElementConfigAcc, [key]: elementMappings };
}, {});
const simulatorDateFormat = "D MMM YYYY HH:mm [GMT]Z";
const worldParameters = {
    hostsConfig,
    pagesConfig,
    pageElementMappings,
    simulatorDateFormat,
};
export const common = {
    import: ['./src/step-definitions/**/*.js'],
    paths: ['./src/features/**/*.feature'],
    worldParameters
};
export default common;
