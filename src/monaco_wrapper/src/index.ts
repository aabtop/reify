import * as monaco from "monaco-editor";

let container = document.getElementById('container');
if (!container) {
  throw new Error('Could not find container element.');
}

let editor = monaco.editor.create(container, {
  // Without explicitly setting this I observed an extra space being added
  // whenever I selected something.
  renderWhitespace: "none",
  // We want the editor to be automatically resized as its container is resized.
  automaticLayout: true,
  value: [
    'function x() {',
    '\tconsole.log("Hello world!");',
    '}'
  ].join('\n'),
  language: 'typescript'
});

declare class QWebChannel {
  constructor(transport: any, initCallback: (channel: QWebChannel) => void);
  objects: any;
}
declare var qt: any;
new QWebChannel(qt.webChannelTransport, function (channel: QWebChannel) {
  var monaco_qt_bridge = channel.objects.monaco_qt_bridge;
  monaco_qt_bridge.dataChanged.connect((x: string) => {
    console.log("hi there");
    editor.setValue(x);
  });
});
