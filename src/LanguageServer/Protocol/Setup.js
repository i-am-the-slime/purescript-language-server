"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const node_1 = require("vscode-languageserver/node");
const vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
exports.initConnection = (commands) => (cb) => () => {
    const connection = node_1.createConnection();
    connection.listen();
    connection.onInitialize((params) => {
        connection.console.info(JSON.stringify(params));
        cb({
            params,
            connection
        })();
        return {
            capabilities: {
                // Tell the client that the server works in FULL text document sync mode
                textDocumentSync: node_1.TextDocumentSyncKind.Full,
                // Tell the client that the server supports code complete
                completionProvider: {
                    resolveProvider: false,
                    triggerCharacters: ["."]
                },
                codeLensProvider: {
                    resolveProvider: false
                },
                hoverProvider: true,
                signatureHelpProvider: {
                    triggerCharacters: [" "],
                    retriggerCharacters: [" "]
                },
                definitionProvider: true,
                typeDefinitionProvider: false,
                workspaceSymbolProvider: true,
                documentSymbolProvider: true,
                codeActionProvider: true,
                executeCommandProvider: (params.initializationOptions || {}).executeCommandProvider === false
                    ? undefined : {
                    commands
                },
                referencesProvider: true,
                foldingRangeProvider: true,
                documentFormattingProvider: true,
                workspace: {
                    fileOperations: {
                        didCreate: { filters: [{ pattern: { glob: "./src/**/*.purs" } }, { pattern: { glob: "./src/**/*.purs" } }] },
                        didRename: { filters: [{ pattern: { glob: "./src/**/*.purs" } }, { pattern: { glob: "./src/**/*.purs" } }] },
                        didDelete: { filters: [{ pattern: { glob: "./src/**/*.purs" } }, { pattern: { glob: "./src/**/*.purs" } }] },
                    }
                }
            }
        };
    });
    return connection;
};
exports.initDocumentStore = (conn) => () => {
    const documents = new node_1.TextDocuments(vscode_languageserver_textdocument_1.TextDocument);
    documents.listen(conn);
    return documents;
};
exports.getConfigurationImpl = (conn) => () => conn.workspace.getConfiguration("purescript").then(config => {
    return { purescript: config };
});
