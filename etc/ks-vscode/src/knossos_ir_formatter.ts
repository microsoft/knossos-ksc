/*
Knossos IR formatter VSCode extension

Original extension vscode-lisp-formatter was developed by Jacob Clark and licensed under the MIT license
https://github.com/imjacobclark/vscode-lisp-formatter

*/

function insertNewline(state: any) {
	const indent = " ".repeat(2 * (getIndent(state) + 1));

	// don't insert an extra line break if it is already on a new line
	if (!state.newLine) {
		state.formattedDocument += "\n" + indent;
		return state;
	}

	// just add the indent
	state.formattedDocument += indent;
	state.newLine = false;

	return state;
}

function checkContext(state: any, contextOp: string, positionLimit: number) {
	if (state.stack.length == 0) {
		return false;
	}
	let index = -1;
	for (var i = state.stack.length - 1; i >= 0; i--) {
		if (state.stack[i].op == contextOp) {
			index = i;
			break;
		}
	}
	return index >= 0 && state.stack[index].argIndex <= positionLimit;
}

function getIndent(state: any) {
	return (state.stack.length > 0) ? state.stack[state.stack.length-1].indent : 0;
}

function needLineBreak(state: any) {
	let currentOp = "";
	let opIndex = -1;
	if (state.stack.length > 0) {
		currentOp = state.stack[state.stack.length-1].op;
		opIndex = state.stack[state.stack.length-1].argIndex;
	}

	const insideIfPred = checkContext(state, "if", 1);
	const insideAssertPred = checkContext(state, "assert", 1);
	const insideDeltaVecDim = checkContext(state, "deltaVec", 2);
	const insideIndexDim = checkContext(state, "index", 1);
	const insideLetBind = checkContext(state, "let", 1);
	const insideDefArgs = checkContext(state, "def", 3);

	return currentOp == "lam" && opIndex == 2
						|| !insideIfPred && !insideAssertPred && !insideDeltaVecDim && !insideIndexDim
							&& (currentOp == "add" 
								|| currentOp == "sub"
								|| currentOp == "mul"
								|| currentOp == "div")
						|| currentOp == "if" && opIndex >= 2
						|| currentOp == "assert" && opIndex >= 2
						|| currentOp == "tuple"
						|| currentOp == "deltaVec" && opIndex == 3
						|| currentOp == "let"
						|| currentOp == "" && insideLetBind
						|| currentOp == "" && insideDefArgs
						|| currentOp == "def"
						|| currentOp == "print";
}

function formatOpenList(state: any, token: string) {
	const charIsEscaped = state.escaped;
	if (charIsEscaped) {
		state.escaped = false;
	}

	if (!state.string && !state.comment) {
		// Increment the argdIndex if no whitespace before opening parenthesis
		if (state.stack.length > 0 && !state.whitespaceEmitted) {
			state.stack[state.stack.length-1].argIndex++;
		}
		const isOnNewLine = needLineBreak(state);

		if (isOnNewLine) {
			insertNewline(state);
		}

		state.formattedDocument += token;
		state.openLists++;

		const currentIndent = getIndent(state);
		state.stack.push({
			op: "",
			argIndex: 0,
			indent: (isOnNewLine) ? currentIndent + 1 : currentIndent
		});
		state.whitespaceEmitted = false;
	} else {
		state.formattedDocument += token;
	}

	return state;
}

function formatCloseList(state: any, token: string) {
	const charIsEscaped = state.escaped;
	if (charIsEscaped) {
		state.escaped = false;
	}

	if (!state.string && !state.comment) {
		state.formattedDocument += token;
		state.openLists--;

		state.stack.pop();
		state.whitespaceEmitted = false;
	} else {
		state.formattedDocument += token;
	}

	return state;
}

function formatNewLine(state: any, token: string) {
	state.newLine = true;
	state.comment = false;
	if (state.stack.length > 0 && !state.whitespaceEmitted) {
		state.whitespaceEmitted = true;
		state.stack[state.stack.length-1].argIndex++;
	}
	state.formattedDocument += token;

	return state;
}

function formatWhitespace(state: any, token: string) {
	const charIsInsideACommentOrString = state.comment || state.string;
	// ignore repeated whitespace characters
	if (charIsInsideACommentOrString || state.stack.length > 0 && !state.whitespaceEmitted) {
		state.formattedDocument += token;

		// increase the argIndex when inside an array
		if (!charIsInsideACommentOrString) {
			state.whitespaceEmitted = true;
			state.stack[state.stack.length-1].argIndex++;
		}
	}

	return state;
}

function formatComment(state: any, token: string) {
	const charIsEscaped = state.escaped;
	if (charIsEscaped) {
		state.escaped = false;
	} else if (!state.string) {
		state.comment = true;
	}

	state.formattedDocument += token;

	return state;
}

function escapeFormatter(state: any, token: string) {
	state.escaped = !state.escaped;
	state.formattedDocument += token;

	return state;
}

function stringFormatter(state: any, token: string) {
	const charIsEscaped = state.escaped;
	if (charIsEscaped) {
		state.escaped = false;
	} else {
		state.string = !state.string;
	}

	// Reset whitespaceEmitted
	state.whitespaceEmitted = false;

	state.formattedDocument += token;

	return state;
}

export function formatKnossosIR(document: string) {
	let state = {
		document: document,
		formattedDocument: "",
		openLists: 0,
		comment: false,
		escaped: false,
		string: false,
		newLine: false,
		array: false,
		whitespaceEmitted: false,
		stack: [] as {op: string, argIndex: number, indent: number}[]
	}

	let formatters: any = {
		"(": formatOpenList,
		")": formatCloseList,
		"\r": formatNewLine,
		"\n": formatNewLine,
		" ": formatWhitespace,
		"\t": formatWhitespace,
		";": formatComment,
		"\\": escapeFormatter,
		"\"": stringFormatter
	}

	for (var i = 0; i < state.document.length; i++) {
		const cursor = state.document.charAt(i)
		const formatter = formatters[cursor];

		if (formatter) {
			state = formatter(state, cursor);
		} else {
			// Uncommenting this will insert line breaks even when the subexpression does not start from parenthesis
			// but I found it a bit too verbose. The current approach of reading one character at a time cannot look
			// ahead and a proper parsing followed by pretty printing will solve this issue.

			// if (state.whitespaceEmitted && !state.comment && !state.string && needLineBreak(state)) {
			//	insertNewline(state);
			//}
			state.formattedDocument += cursor;
			if (state.stack.length > 0) {
				let currentOp = state.stack[state.stack.length-1];
				if (currentOp.argIndex == 0) {
					currentOp.op += cursor;
				}
			}
			state.newLine = false;
			// reset the whitespceEmitted variable if not in string or comment
			if (!state.comment && !state.string) {
				state.whitespaceEmitted = false;
			}
			if (state.escaped) {
				state.escaped = false;
			}
		}
	}

	return state.formattedDocument;
}