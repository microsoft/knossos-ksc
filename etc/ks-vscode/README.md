# Knossos ks-lisp

A VS Code extension for the Knossos lisp-like IR

## Requirements

None

<!---

## Extension Settings

Include if your extension adds any VS Code settings through the `contributes.configuration` extension point.

For example:

This extension contributes the following settings:

* `myExtension.enable`: enable/disable this extension
* `myExtension.thing`: set to `blah` to do something

--->

## How to build the formatter extension

1. Install npm from https://nodejs.org/en/download/
2. In `./etc/ks-vscode` run

```
npm install
```

3. Then run

```
npm run compile
```

4. Copy the files in `./etc/ks-vscode/` to `C:\Users\<user>\.vscode\extensions\knossos-vscode-0.01` 

```
.
├── package.json
├── language-configuration.json
├── out
|   ├── extension.js
|   └── knossos_ir_formatter.js
└── syntaxes
    └── Knossos.tmLanguage
```

5. Start a new instance of VS Code. Open a `.ks` file. Make sure that VS Code auto detects Knossos IR. Then try "Format Document" (`shift` + `alt` + `F`).
## Known Issues

None yet.

## Release Notes

### 1.0.0

Initial release.

