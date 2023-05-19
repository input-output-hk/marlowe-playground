import { BlockBase, BlockCreate, BlockChange } from "blockly/core/events/events";

export function createBlocklyInstance_ () {
  return import("blockly");
};

export type Blockly = Awaited<ReturnType<typeof createBlocklyInstance_>>;
export type Workspace = InstanceType<Blockly["Workspace"]>
export type WorkspaceSvg = InstanceType<Blockly["WorkspaceSvg"]>
export type Block = InstanceType<Blockly["Block"]>

export type TZInfo = {tzOffset: number, offsetString: string}

export function isBlockEvent (event: unknown): event is BlockBase {
  return event.hasOwnProperty("blockId");
}

export function isBlockCreateEvent (event: unknown): event is BlockCreate {
  return event.hasOwnProperty("type") && event["type"] == "create";
}
export function isBlockChangeEvent (event: unknown): event is BlockChange {
  return event.hasOwnProperty("type") && event["type"] == "change";
}
