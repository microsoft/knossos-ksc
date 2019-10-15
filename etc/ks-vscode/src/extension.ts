/*
Knossos IR formatter VSCode extension

Original extension vscode-lisp-formatter was developed by Jacob Clark and licensed under the MIT license
https://github.com/imjacobclark/vscode-lisp-formatter

*/

import * as vscode from 'vscode';
import { formatKnossosIR } from './knossos_ir_formatter'

function getFullDocRange(document: vscode.TextDocument): vscode.Range {
	return document.validateRange(
		new vscode.Range(
			new vscode.Position(0, 0),
			new vscode.Position(Number.MAX_VALUE, Number.MAX_VALUE)
		)
	);
}

export function activate(context: vscode.ExtensionContext) {
	vscode.languages.registerDocumentFormattingEditProvider('ks-lisp', {
		provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.TextEdit[] {

			return [vscode.TextEdit.replace(
				getFullDocRange(document),
				formatKnossosIR(document.getText()))];
		}
	});
}
