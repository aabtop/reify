import * as monaco from "monaco-editor";

let container = document.getElementById('container');
if (!container) {
  throw new Error('Could not find container element.');
}

monaco.editor.create(container, {
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
