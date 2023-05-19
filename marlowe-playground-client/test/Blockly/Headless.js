import { registerDateTimeField } from "../../src/Blockly/DateTimeField.js";

export function createBlocklyInstance_() {
  return require("blockly/blockly-node");
}

export function createWorkspace_(blockly) {
  return new blockly.Workspace();
}

export function initializeWorkspace_(blockly, workspace) {
  try {
    blockly.Extensions.register("hash_validator", function () {});
  } catch (err) {}
  try {
    blockly.Extensions.register("number_validator", function () {});
  } catch (err) {}
  try {
    registerDateTimeField(blockly);
  } catch (err) {}
  try {
    blockly.Extensions.register("dynamic_timeout_type", function () {});
  } catch (err) {}

  var xmlText =
    '<xml id="workspaceBlocks" style="display:none"><block type="BaseContractType" x="13" y="187" id="root_contract"></block></xml>';
  var workspaceBlocks = blockly.Xml.textToDom(xmlText);
  blockly.Xml.domToWorkspace(workspaceBlocks, workspace);
  workspace.getAllBlocks()[0].setDeletable(false);
}

export function newBlock_(workspace, name) {
  return workspace.newBlock(name);
}
