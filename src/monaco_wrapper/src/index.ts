import * as monaco from "monaco-editor";

let maybe_container = document.getElementById('container');
if (!maybe_container) {
  throw new Error('Could not find container element.');
}

let container = maybe_container;

let lastSavedVersionId = -1;
let fileIsDirty = false;
let supressDirtyNotifications = false;

function CreateEditor(extraLibs: { filepath: string, content: string }[]) {
  // It's not clear why, but setting the extra libs directly in the
  // `setExtraLibs` call doesn't seem to work as expected.
  monaco.languages.typescript.typescriptDefaults.setExtraLibs([]);
  extraLibs.map(
    (x) => {
      monaco.languages.typescript.typescriptDefaults.addExtraLib(x.content, "inmemory://model/" + x.filepath);
    }
  );
  monaco.languages.typescript.typescriptDefaults.setCompilerOptions({
    module: monaco.languages.typescript.ModuleKind.ES2015,
    target: monaco.languages.typescript.ScriptTarget.ES2015,
    lib: [
      "es5",
      "es2015.core",
      "es2015.symbol",
      "es2015.iterable",
      "es2015.generator",
    ],
    strict: true,
    allowNonTsExtensions: true,
  });

  return monaco.editor.create(container, {
    // Without explicitly setting this I observed an extra space being added
    // whenever I selected something.
    renderWhitespace: "none",
    // We want the editor to be automatically resized as its container is resized.
    automaticLayout: true,
    value: [
      'import * as h from \'hypo\';',
      '',
      'export function Torus() {',
      '  return h.Torus({',
      '    inner_radius: 2,',
      '    outer_radius: 3,',
      '    num_cross_section_points: 10,',
      '    num_slices: 20',
      '  });',
      '}',
      '',
    ].join('\n'),
    language: 'typescript',
    theme: 'vs-dark',
  });
}

let current_filepath = '';
var editor: monaco.editor.IStandaloneCodeEditor | null = null;

declare class QWebChannel {
  constructor(transport: any, initCallback: (channel: QWebChannel) => void);
  objects: any;
}
declare var qt: any;
new QWebChannel(qt.webChannelTransport, function (channel: QWebChannel) {
  var monaco_qt_bridge = channel.objects.monaco_qt_bridge;

  monaco_qt_bridge.TypeScriptWrapperConstructor.connect((modules: string[][]) => {
    const extraLibs = modules.map((x) => {
      return {
        filepath: x[0],
        content: x[1],
      };
    });

    editor = CreateEditor(extraLibs);
    let model = editor.getModel()!;
    lastSavedVersionId = model.getAlternativeVersionId();
    model.onDidChangeContent((e) => {
      if (supressDirtyNotifications) {
        return;
      }

      const fileWasDirty = fileIsDirty;
      fileIsDirty = (lastSavedVersionId !== model.getAlternativeVersionId());
      if (fileWasDirty !== fileIsDirty) {
        monaco_qt_bridge.FileDirtyStatusChanged(fileIsDirty);
      }
    });
  });

  monaco_qt_bridge.NewFile.connect(() => {
    if (!editor) {
      throw new Error('Editor not created yet while calling NewFile().');
    }
    current_filepath = '';
    lastSavedVersionId = -1;
    editor.setValue('');
  });

  monaco_qt_bridge.SaveAs.connect((filepath: string) => {
    if (!editor) {
      throw new Error('Editor not created yet while calling SaveAs().');
    }
    current_filepath = filepath;
    lastSavedVersionId = editor.getModel()!.getAlternativeVersionId();
    fileIsDirty = false;
    monaco_qt_bridge.SaveAsReply(filepath, editor.getValue());
  });

  monaco_qt_bridge.QueryContent.connect(() => {
    if (!editor) {
      throw new Error('Editor not created yet while calling QueryContent().');
    }
    monaco_qt_bridge.QueryContentReply(editor.getValue());
  });

  monaco_qt_bridge.Open.connect((filepath: string, content: string) => {
    if (!editor) {
      throw new Error('Editor not created yet while calling Open().');
    }
    current_filepath = filepath;
    supressDirtyNotifications = true;
    editor.setValue(content);
    lastSavedVersionId = editor.getModel()!.getAlternativeVersionId();
    fileIsDirty = false;
    supressDirtyNotifications = false;
  });

  monaco_qt_bridge.WebChannelInitialized();
});
