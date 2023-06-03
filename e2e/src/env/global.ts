export type PageId = string;
export type PagesConfig = Record<PageId, Record<string, string>>;
export type HostsConfig = Record<string, string>;
export type ValidAccessibilityRoles = "link" | "button" | "group" | "heading" | "spinbutton" | "textbox";
export type ElementKey = string;
export type ElementLocator = Record<string, string>;
export type PageElementMappings = Record<PageId, Record<ElementKey, ElementLocator>>;
export type FixtureKey = string;
export type FixtureId= string;
export type FixtureMappings = Record<FixtureId, string>;
export type DateTimeFormat = string;

export type GlobalConfig = {
  hostsConfig: HostsConfig;
  pagesConfig: PagesConfig;
  pageElementMappings: PageElementMappings;
  fixtureMappings: FixtureMappings;
  simulatorDateFormat: DateTimeFormat;
}