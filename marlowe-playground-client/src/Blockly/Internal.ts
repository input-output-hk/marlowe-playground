import * as jsonBigInt from "json-bigint";
// import { registerDateTimeField } from "src/Blockly/DateTimeField.js";
import { registerDateTimeField } from "./DateTimeField.js";
const JSONbig = jsonBigInt({ useNativeBigInt: true });

export function createBlocklyInstance_ () {
  return import("blockly");
};

type Blockly = Awaited<ReturnType<typeof createBlocklyInstance_>>;
type Workspace = InstanceType<Blockly["Workspace"]>
type WorkspaceSvg = InstanceType<Blockly["WorkspaceSvg"]>

// w.constructor
// type Workspace2 = typeof (Blockly["Workspace"].constructor)


export const debugBlockly = (name) => (state) => () => {
  if (typeof (window as any).blockly === "undefined") {
    (window as any).blockly = {};
  }
  (window as any).blockly[name] = state;
};

type TZInfo = {tzOffset: number, offsetString: string}
export const createWorkspace =
  (blockly: Blockly) => (workspaceDiv: string) => (config) => (tzInfo: TZInfo) => () => {

    /* Disable comments */
    try {
      blockly.ContextMenuRegistry.registry.unregister("blockComment");
    } catch (err) {}

    /* Disable disabling blocks */
    try {
      blockly.ContextMenuRegistry.registry.unregister("blockDisable");
    } catch (err) {}

    /* Register extensions */
    /* Silently clean if already registered */
    try {
      blockly.Extensions.register("hash_validator", function () {});
    } catch (err) {}
    blockly.Extensions.unregister("hash_validator");
    try {
      blockly.Extensions.register("number_validator", function () {});
    } catch (err) {}
    blockly.Extensions.unregister("number_validator");
    try {
      blockly.Extensions.register("dynamic_timeout_type", function () {});
    } catch (err) {}
    blockly.Extensions.unregister("dynamic_timeout_type");

    /* Hash extension (advanced validation for the hash fields) */
    blockly.Extensions.register("hash_validator", function () {
      var thisBlock = this;

      /* Validator for hash */
      var hashValidator = function (input) {
        var cleanedInput = input
          .replace(new RegExp("[^a-fA-F0-9]+", "g"), "")
          .toLowerCase();
        if (new RegExp("^([a-f0-9][a-f0-9])*$", "g").test(cleanedInput)) {
          return cleanedInput;
        } else {
          return null;
        }
      };

      ["currency_symbol"].forEach(function (fieldName) {
        var field = thisBlock.getField(fieldName);
        if (field != null) {
          field.setValidator(hashValidator);
        }
      });
    });

    /* Number extension (advanced validation for number fields - other than timeout) */
    blockly.Extensions.register("number_validator", function () {
      var thisBlock = this;

      /* Validator for number fields */
      var numberValidator = function (input) {
        if (!isFinite(input)) {
          return null;
        }
      };

      thisBlock.inputList.forEach((input) => {
        input.fieldRow.forEach((field) => {
          if (field instanceof blockly.FieldNumber) {
            field.setValidator(numberValidator);
          }
        });
      });
    });

    const FieldDateTime = registerDateTimeField(blockly);

    // This extension takes care of changing the `timeout field` depending on the value of
    // `timeout_type`. When `timeout_type` is a constant, then we show a datetime picker
    // if it is a parameter we show a text field.
    blockly.Extensions.register("dynamic_timeout_type", function () {
      const timeoutTypeField = this.getField("timeout_type");
      // The timeoutField is mutable as we change it depending of the value of
      // the timeout type.
      let timeoutField = this.getField("timeout");
      // The field Row is what groups a line in the block. In the case of a when block
      // this is ["After" label, timeoutTypeField, timeoutField]
      const row = timeoutField.getParentInput();
      const safeRemoveField = function (fieldName) {
        if (row.fieldRow.findIndex((field) => field.name === fieldName) > -1) {
          row.removeField(fieldName);
        }
      };
      // We store in this mutable data the values of the timeout field indexed by the different
      // timeout types. We initialize this as undefined as there is no blockly event to get the initial
      // loaded data, so we mark this information to be gathered on a different way.
      let fieldValues = undefined; // { time :: String | undefined, time_param :: String };

      // The onChange function lets you know about Blockly events of the entire workspace, visual
      // changes, data changes, etc.
      const thisBlock = this;
      this.setOnChange(function (event) {
        // we only care about events for this block.
        if (event.blockId != thisBlock.id) return;

        timeoutField = thisBlock.getField("timeout");

        // This function sets the Timeout Field of the correct type
        const updateTimeoutField = function (type) {
          if (type == "time") {
            safeRemoveField("timeout");
            row.appendField(
              new FieldDateTime(fieldValues["time"], undefined, tzInfo),
              "timeout"
            );
          } else if (type == "time_param") {
            safeRemoveField("timeout");
            row.appendField(
              new blockly.FieldTextInput(fieldValues["time_param"]),
              "timeout"
            );
            // For some reason Blockly doens't automatically fire this event
            // indicating that the timeout field has changed. Not firing the
            // event results in a bug where if you attach a new When block,
            // change to time_param and convert to marlowe, the old time value
            // is presented.
            console.log('yea yea2')
            debugger;
            blockly.Events.CHANGE
            blockly.Events.fire(
              new blockly.Events.Change(
                thisBlock, // block that changed
                "field", // type of element that changed
                "timeout", // name of the element that changed
                fieldValues["time"], // old value
                fieldValues["time_param"] // new value
              )
            );
          }
        };

        // For the first event we receive, we set the fieldValues to whatever is stored in
        // the timeoutField.
        if (typeof fieldValues === "undefined") {
          const type = timeoutTypeField.getValue();
          const val = timeoutField.getValue();

          fieldValues = {
            // If the timeout type was set to constant, then set the value here and a sensible
            // default for time_param
            time: type == "time" ? val : undefined,
            // If the timeout type was set to a time parameter, then set the value here and
            // use undefined for `time`. That will result than on the first switch to a Constant, the
            // current time will be used.
            time_param: type == "time_param" ? val : "time_param",
          };
          // Set the timeout field to the correct type
          updateTimeoutField(type);
        }

        if (event.element == "field" && event.name == "timeout") {
          // If the timeout field changes, update the fieldValues "local store"
          fieldValues[timeoutTypeField.getValue()] = event.newValue;
        } else if (event.element == "field" && event.name == "timeout_type") {
          // If the timeout_type field changes, then update the timeout field
          updateTimeoutField(event.newValue);
        }
      });
    });

    /* Inject workspace */
    var workspace = blockly.inject(workspaceDiv, config);
    blockly.svgResize(workspace);

    return workspace;
  };

export const resize = (blockly: Blockly) => (workspace: WorkspaceSvg) => () => {
  blockly.svgResize(workspace);
  workspace.render();
};

function removeUndefinedFields(obj) {
  for (var propName in obj) {
    if (obj[propName] === undefined) {
      delete obj[propName];
    }
  }
}

function removeEmptyArrayFields(obj) {
  for (var propName in obj) {
    if (Array.isArray(obj[propName]) && obj[propName].length == 0) {
      delete obj[propName];
    }
  }
}

export const addBlockType_ = (blockly: Blockly) => (name: string) => (block) => () => {
  // we really don't want to be mutating the input object, it is not supposed to be state
  var clone = JSONbig.parse(JSONbig.stringify(block));
  removeUndefinedFields(clone);
  removeEmptyArrayFields(clone);
  blockly.Blocks[name] = {
    init: function () {
      this.jsonInit(clone);
    },
  };
};

export const initializeWorkspace_ =
  (blockly: Blockly) => (workspace: WorkspaceSvg) => (workspaceBlocks) => () => {
    blockly.Xml.domToWorkspace(workspaceBlocks, workspace);
    workspace.getAllBlocks(false)[0].setDeletable(false);
  };

export const render = (workspace: WorkspaceSvg) => () => {
  workspace.render();
};

export const getBlockById_ =
  (just) => (nothing) => (workspace: WorkspaceSvg) => (id) => () => {
    var result = workspace.getBlockById(id);
    if (result) {
      return just(result);
    } else {
      return nothing;
    }
  };

// FIXME WorkspaceSvg
export const workspaceXML = (blockly: Blockly) => (workspace ) => () => {
  const isEmpty = workspace.getAllBlocks()[0].getChildren().length == 0;
  if (isEmpty) {
    return "";
  } else {
    var dom = blockly.Xml.workspaceToDom(workspace);
    return blockly.utils.xml.domToText(dom);
  }
};

export const loadWorkspace = (blockly: Blockly) => (workspace) => (xml) => () => {
  var dom = blockly.utils.xml.textToDomDocument(xml) as any; // TODO FIXME
  blockly.Xml.clearWorkspaceAndLoadFromXml(dom.childNodes[0], workspace);
  workspace.getAllBlocks(false)[0].setDeletable(false);
};

export const addChangeListener = (workspace) => (listener) => () => {
  workspace.addChangeListener(listener);
};

export const removeChangeListener = (workspace) => (listener) => () => {
  workspace.removeChangeListener(listener);
};

export const workspaceToDom = (blockly: Blockly) => (workspace) => () => {
  return blockly.Xml.workspaceToDom(workspace);
};

export const select = (block) => () => {
  block.select();
};

export const centerOnBlock = (workspace) => (blockId) => () => {
  workspace.centerOnBlock(blockId);
};

export const hideChaff = (blockly: Blockly) => () => {
  blockly.hideChaff();
};

export const getBlockType = (block) => {
  return block.type;
};

export const updateToolbox_ = (toolboxJson) => (workspace) => () => {
  workspace.updateToolbox(toolboxJson);
};

export const clearUndoStack = (workspace) => () => {
  workspace.clearUndo();
};

export const isWorkspaceEmpty = (workspace) => () => {
  var topBlocks = workspace.getTopBlocks(false);
  return topBlocks == null || topBlocks.length == 0;
};

export const setGroup = (blockly: Blockly) => (isGroup) => () =>
  blockly.Events.setGroup(isGroup);

export const inputList = (block) => {
  return block.inputList;
};

export const connectToPrevious = (block) => (input) => () => {
  block.previousConnection.connect(input.connection);
};
export const previousConnection = (block) => {
  return block.previousConnection;
};

export const nextConnection = (block) => {
  return block.nextConnection;
};

export const connect = (from) => (to) => () => {
  from.connect(to);
};

export const connectToOutput = (block) => (input) => () => {
  block.outputConnection.connect(input.connection);
};

export const newBlock = (workspace) => (name) => () => {
  var block = workspace.newBlock(name);
  block.initSvg();
  return block;
};

export const inputName = (input) => {
  return input.name;
};

export const inputType = (input) => {
  return input.type;
};

export const clearWorkspace = (workspace) => () => {
  workspace.clear();
};

export const fieldRow = (input) => {
  return input.fieldRow;
};

export const setFieldText = (field) => (text) => () => {
  field.setValue(text);
};

export const fieldName = (field) => {
  return field.name;
};
