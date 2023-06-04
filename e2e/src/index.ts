// The purpose of this file is to be able to enable dynamic paramters to be passed via the actual cucumber arguments 
// that we pass when calling the cucumber executable.

import dotenv from 'dotenv';
import { env, getJsonFromFile } from './env/parseEnv';
import {
  GlobalConfig,
  HostsConfig,
  PagesConfig,
  PageElementMappings,
  DateTimeFormat,
} from './env/global';
import * as fs from 'fs';

dotenv.config({path: env('COMMON_CONFIG_FILE')})
const hostsConfig: HostsConfig = getJsonFromFile(env('HOSTS_URLS_PATH'))
const pagesConfig: PagesConfig = getJsonFromFile(env('PAGE_URLS_PATH'))
const pageMappingFiles = fs.readdirSync(`${process.cwd()}${env('PAGE_ELEMENT_MAPPINGS_PATH')}`)

const pageElementMappings: PageElementMappings = pageMappingFiles.reduce(
  (pageElementConfigAcc, file) => {
    const key = file.replace('.json', '');
    const elementMappings = getJsonFromFile(`${env('PAGE_ELEMENT_MAPPINGS_PATH')}${file}`);
    return { ...pageElementConfigAcc, [key]: elementMappings}
  },
  {}
);

const simulatorDateFormat: DateTimeFormat = "D MMM YYYY HH:mm [GMT]Z";

const worldParameters: GlobalConfig = {
  hostsConfig,
  pagesConfig,
  pageElementMappings,
  simulatorDateFormat,
};

const common = `./src/features/**/*.feature \
                --require-module ts-node/register \
                --require ./src/step-definitions/**/**/*.ts \
                --world-parameters ${JSON.stringify(worldParameters)} \
                -f json:./reports/reports.json \
                --format progress-bar`;

const dev = `${common} --tags '@dev'`;
const smoke = `${common} --tags '@smoke'`;
const regression = `${common} --tags '@regression'`;
const wip = `${common} --tags '@wip'`;

export { dev, smoke, regression , wip }