"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.filenameToUri = exports.uriToFilename = void 0;
const vscode_uri_1 = require("vscode-uri");
const uriToFilename = (uri) => () => vscode_uri_1.URI.parse(uri).fsPath;
exports.uriToFilename = uriToFilename;
const filenameToUri = (filename) => () => vscode_uri_1.URI.file(filename).toString();
exports.filenameToUri = filenameToUri;
