export function isWarning_(severity) {
  return severity == 4;
}

export function isError_(severity) {
  return severity == 8;
}

export function getMonaco() {
  return globalThis.monaco;
}

export function registerLanguage_(monaco, language) {
  monaco.languages.register(language);
}

export function defineTheme_(monaco, theme) {
  monaco.editor.defineTheme(theme.name, theme.themeData);
}

export function setMonarchTokensProvider_(monaco, languageId, languageDef) {
  return monaco.languages.setMonarchTokensProvider(languageId, languageDef);
}

export function setModelMarkers_(monaco, model, owner, markers) {
  monaco.editor.setModelMarkers(model, owner, markers);
}

export function getModelMarkers_(monaco, model) {
  return monaco.editor.getModelMarkers({ resource: model.uri });
}

export function create_(monaco, nodeId, languageId) {
  const editor = monaco.editor.create(nodeId, {
    language: languageId,
    minimap: {
      enabled: false,
    },
  });

  window.addEventListener("resize", function () {
    editor.layout();
  });

  return editor;
}

export function setTheme_(monaco, themeName) {
  monaco.editor.setTheme(themeName);
}

export function onDidChangeContent_(editor, handler) {
  editor.getModel().onDidChangeContent(function (event) {
    handler(event)();
  });
}

export function addExtraTypeScriptLibsJS_(monaco) {
  globalThis.monacoExtraTypeScriptLibs.forEach(function ([dts, dtsFilename]) {
    monaco.languages.typescript.typescriptDefaults.addExtraLib(
      dts,
      dtsFilename
    );
  });
}

export function setStrictNullChecks_(monaco, bool) {
  var compilerOptions =
    monaco.languages.typescript.typescriptDefaults.getCompilerOptions();
  compilerOptions["strictNullChecks"] = bool;
  monaco.languages.typescript.typescriptDefaults.setCompilerOptions(
    compilerOptions
  );
}

export function getDecorationRange_(editor, identifier) {
  return editor.getDecorationRange(identifier);
}

export function setDeltaDecorations_(editor, oldDecorations, newDecorations) {
  return editor.deltaDecorations(oldDecorations, newDecorations);
}

export function getModel_(editor) {
  return editor.getModel();
}

export function getEditorId_(editor) {
  return editor.getId();
}

export function getValue_(model) {
  return model.getValue();
}

export function setValue_(model, value) {
  return model.setValue(value);
}

export function getLineCount_(model) {
  return model.getLineCount();
}

export function setTokensProvider_(monaco, languageId, provider) {
  return monaco.languages.setTokensProvider(languageId, provider);
}

export function completionItemKind_(name) {
  return monaco.languages.CompletionItemKind[name];
}

export function markerSeverity_(name) {
  return monaco.MarkerSeverity[name];
}

export function registerHoverProvider_(monaco, languageId, provider) {
  return monaco.languages.registerHoverProvider(languageId, provider);
}

export function registerCompletionItemProvider_(monaco, languageId, provider) {
  return monaco.languages.registerCompletionItemProvider(languageId, provider);
}

export function registerCodeActionProvider_(
  monaco,
  languageId,
  actionProvider
) {
  return monaco.languages.registerCodeActionProvider(
    languageId,
    actionProvider
  );
}

export function registerDocumentFormattingEditProvider_(
  monaco,
  languageId,
  formatter
) {
  return monaco.languages.registerDocumentFormattingEditProvider(
    languageId,
    formatter
  );
}

export function setPosition_(editor, position) {
  editor.setPosition(position);
}

export function revealRange_(editor, range) {
  editor.revealRange(range);
}

export function revealRangeInCenter_(editor, range) {
  editor.revealRangeInCenter(range);
}

export function revealRangeAtTop_(editor, range) {
  editor.revealRangeAtTop(range);
}

export function revealRangeNearTop_(editor, range) {
  editor.revealRangeNearTop(range);
}

export function revealLine_(editor, lineNumber) {
  editor.revealLine(lineNumber);
}

export function layout_(editor) {
  editor.layout();
}

export function focus_(editor) {
  editor.focus();
}

export function enableVimBindings_(editor) {
  var statusNode = document.getElementById("statusline");
  var vimMode = globalThis.initVimMode(editor, statusNode);
  return () => vimMode.dispose();
}

export function enableEmacsBindings_(editor) {
  var emacsMode = new globalThis.EmacsExtension(editor);
  emacsMode.start();
  return () => emacsMode.dispose();
}

export function completionItemKindEq_(a, b) {
  return a == b;
}

export function completionItemKindOrd_(lt, eq, gt, a, b) {
  if (a < b) {
    return lt;
  } else if (a == b) {
    return eq;
  } else {
    return gt;
  }
}

export function setReadOnly_(editor, val) {
  editor.updateOptions({ readOnly: val });
}

export function dispose_(disposable) {
  disposable.dispose();
}
