import { Connection, createConnection, InitializeParams, TextDocuments, TextDocumentSyncKind } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
exports.initConnection = (commands: string[]) => (cb: (arg: { params: InitializeParams, connection: Connection }) => () => void) => (): Connection => {
    const connection = createConnection();
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
                textDocumentSync: TextDocumentSyncKind.Full,
                // Tell the client that the server supports code complete
                completionProvider: {
                    resolveProvider: false,
                    triggerCharacters: ["."]
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
                        didCreate: { filters: [{pattern: { glob: "./src/**/*.purs"}}, {pattern: { glob: "./src/**/*.purs"}}]},
                        didRename: { filters: [{pattern: { glob: "./src/**/*.purs"}}, {pattern: { glob: "./src/**/*.purs"}}]},
                        didDelete: { filters: [{pattern: { glob: "./src/**/*.purs"}}, {pattern: { glob: "./src/**/*.purs"}}]},
                    }
                }
                
            }
        };
    });
    return connection;
}

exports.initDocumentStore = (conn: Connection) => () => {
    const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
    documents.listen(conn);
    return documents;
}

exports.getConfigurationImpl = (conn: Connection) => () =>
    conn.workspace.getConfiguration("purescript").then(config => {
        return { purescript: config };
    });
