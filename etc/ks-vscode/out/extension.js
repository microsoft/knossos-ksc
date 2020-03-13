"use strict";
/*
Knossos IR formatter VSCode extension

Original extension vscode-lisp-formatter was developed by Jacob Clark and licensed under the MIT license
https://github.com/imjacobclark/vscode-lisp-formatter

*/
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const knossos_ir_formatter_1 = require("./knossos_ir_formatter");
function getFullDocRange(document) {
    return document.validateRange(new vscode.Range(new vscode.Position(0, 0), new vscode.Position(Number.MAX_VALUE, Number.MAX_VALUE)));
}
function activate(context) {
    vscode.languages.registerDocumentFormattingEditProvider('ks-lisp', {
        provideDocumentFormattingEdits(document) {
            return [vscode.TextEdit.replace(getFullDocRange(document), knossos_ir_formatter_1.formatKnossosIR(document.getText()))];
        }
    });
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map