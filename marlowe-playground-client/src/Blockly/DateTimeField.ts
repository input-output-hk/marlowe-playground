import { Blockly, TZInfo } from "./Blockly";
// This function is analogue to Component.DateTimeLocalInput.State (parseInput)
// It parses the valueStr using the same Regexp and tries to create a Date
// object assuming that the date is in UTC. Functions utcToLocal/localToUtc
// should be used to adjust the offset.
function parseInput(valueStr: string) {
  const timeRegex = /(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})(:(\d{2}))?/;
  const found = timeRegex.exec(valueStr);
  if (found) {
    const date = new Date(`${found[0]}Z`);
    return {
      matchStr: found[0],
      unixTime: date.getTime(), // ms from epoch
    };
  }
  return null;
}

function localToUtc(tzOffset: number, unixTimeLocal: number) {
  return unixTimeLocal + tzOffset * 60 * 1000;
}

function utcToLocal(tzOffset: number, unixTimeUtc: number) {
  return localToUtc(-tzOffset, unixTimeUtc);
}

function showNormalizedDateTime(unixTime: number, trimSeconds: boolean) {
  const timeRegex = trimSeconds
    ? /(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})/
    : /(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2})(:(\d{2}))?/;
  const fullStr = new Date(unixTime).toISOString();
  const found = timeRegex.exec(fullStr);
  if (found) return found[0];
  throw "showNormalizedDateTime can normalize a datetime string"; // This should not happen.
}

function validateFieldDateTimeValue(newValue: number | bigint | string) {
  if (typeof newValue == "number" || typeof newValue == "bigint") {
    return newValue;
  } else if (typeof newValue == "string") {
    const parsedInt = parseInt(newValue, 10);
    if (parsedInt + "" == newValue) {
      return parsedInt;
    }
  }

  return null;
}

// I've separated the creation of the class from the registering in Blockly to get the
// proper types. At some point we should try to modify how Blockly gets loaded so we can
// define this class as a top level construct.
function createDateTimeFieldClass(Blockly: Blockly) {
  const Field = Blockly.Field;

  class FieldDateTime extends Field {
    tzOffset: number;
    offsetString: string;
    // FIXME
    onInputWrapper_: any;

    SERIALIZABLE = true;
    CURSOR = "text";
    picker_: HTMLInputElement;
    foreignObject: SVGForeignObjectElement;
    offsetLabel: SVGTextElement;

    constructor(value: number | bigint | string, validator, tzInfo?: TZInfo) {
      super(validateFieldDateTimeValue(value), validator);
      if (typeof tzInfo === "undefined") {
        tzInfo = {
          tzOffset: 0,
          offsetString: "",
        };
      }
      this.tzOffset = tzInfo.tzOffset;
      this.offsetString = tzInfo.offsetString;

      // The default value for this field is the current date
      FieldDateTime.prototype.DEFAULT_VALUE = validateFieldDateTimeValue(
        new Date().getTime()
      );

      this.onInputWrapper_ = null;
    }

    static fromJson(options) {
      return new FieldDateTime(options["date"], undefined);
    }

    initView() {
      const constants = this.getConstants();
      // The picker element is a native HTML DOM input
      // https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/datetime-local
      this.picker_ = document.createElement("input");
      this.picker_.type = "datetime-local";

      // We add these styles to avoid the picker element from growing
      // when the screen resize
      this.picker_.style.height = `${constants.FIELD_TEXT_HEIGHT}px`;
      this.picker_.style.position = "absolute";
      this.picker_.style.fontSize = `${constants.FIELD_TEXT_FONTSIZE}px`;
      this.picker_.style.boxSizing = "content-box";

      // This hardcoded number was reached by experimentation, if not set
      // the picker will occupy more space than needed. An alternative
      // solution would be to create a canvas node with a 2D context,
      // create a text element with the expected number of characters
      // and calculating the size of that. This is easier.
      this.picker_.style.width = "120px";

      // We convert the UTC unix time to a local diff from epoch
      // and convert that to the normalized representation that the
      // datetime-local object needs (e.g: "2022-03-18T21:42")
      this.picker_.value = showNormalizedDateTime(
        utcToLocal(this.tzOffset, this.getValue()),
        true
      );

      // In order to show an HTML element in Blockly we need to wrap
      // it on an Svg foreignObject
      const foreignObject = document.createElementNS(
        "http://www.w3.org/2000/svg",
        "foreignObject"
      );
      foreignObject.appendChild(this.picker_);
      this.foreignObject = foreignObject;
      // We add the foreignObject to the fields `g` element.
      this.fieldGroup_.appendChild(foreignObject);

      // This element is a textual representation of the local offset
      // (e.g: "GMT+2")
      this.offsetLabel = Blockly.utils.dom.createSvgElement(
        Blockly.utils.Svg.TEXT,
        {
          class: "blocklyText",
        },
        this.fieldGroup_
      );
      this.offsetLabel.textContent = this.offsetString;
      this.offsetLabel.style.fill = "white";
      this.offsetLabel.style.fontSize = `${constants.FIELD_TEXT_FONTSIZE}pt`;
    }

    updateSize_() {
      const constants = this.getConstants();
      let totalWidth = 0;
      const padding = 4;
      const pickerW = this.picker_.offsetWidth;
      const pickerH = constants.FIELD_TEXT_HEIGHT + padding;

      // In order for the input to be displayed, we need to set
      // the foreignObject size to have its children dimensions
      this.foreignObject.setAttribute("width", String(pickerW));
      this.foreignObject.setAttribute("height", String(pickerH));
      totalWidth += pickerW + padding;
      this.offsetLabel.setAttribute("x", String(pickerW + padding));

      var halfHeight = pickerH / 2;

      this.offsetLabel.setAttribute(
        "y",
        String(
          halfHeight -
            constants.FIELD_TEXT_HEIGHT / 2 +
            constants.FIELD_TEXT_BASELINE
        )
      );
      const offsetLabelWidth = this.offsetLabel.getBBox().width;

      totalWidth += offsetLabelWidth + padding;

      this.size_.width = totalWidth;
      this.size_.height = pickerH;
    }

    bindEvents_() {
      super.bindEvents_();
      const thisField = this;

      // Whenever the native input element changes it value, update
      // the field model
      this.onInputWrapper_ = Blockly.bindEventWithChecks_(
        this.picker_,
        "input",
        this,
        function () {
          const parsedValue = parseInput(thisField.picker_.value);
          if (parsedValue) {
            thisField.setValue(
              localToUtc(thisField.tzOffset, parsedValue.unixTime)
            );
          }
        }
      );
    }

    dispose() {
      super.dispose();
      if (this.onInputWrapper_) {
        Blockly.unbindEvent_(this.onInputWrapper_);
      }
    }
  }
  return FieldDateTime;
}

type FieldDateTimeClass = ReturnType<typeof createDateTimeFieldClass>
// Register a custom DateTime Field to blockly:
// Used as base:
// * https://developers.google.com/blockly/guides/create-custom-blocks/fields/customizing-fields/creating
// * https://github.com/google/blockly-samples/blob/master/plugins/field-date/src/field_date.js
export function registerDateTimeField(Blockly: Blockly): FieldDateTimeClass {
  // If the field is already registered, return it.
  const RegisteredFieldDateTime = Blockly.registry
    ? Blockly.registry.getClass("field", "field_datetime")
    : undefined;
  if (RegisteredFieldDateTime) return RegisteredFieldDateTime as FieldDateTimeClass;

  let FieldDateTime = createDateTimeFieldClass(Blockly);

  Blockly.fieldRegistry.register("field_datetime", FieldDateTime);

  return FieldDateTime;
}
