import * as monaco from "monaco-editor";

let container = document.getElementById('container');
if (!container) {
  throw new Error('Could not find container element.');
}

monaco.editor.create(container, {
  value: [
    'function x() {',
    '\tconsole.log("Hello world!");',
    '}'
  ].join('\n'),
  language: 'typescript'
});
