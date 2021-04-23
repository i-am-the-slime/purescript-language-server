import { RequestHandler, RequestHandler0, NotificationHandler, NotificationHandler0, Connection, PublishDiagnosticsParams, WorkspaceEdit } from 'vscode-languageserver';
import { NotificationType0 } from 'vscode-jsonrpc';
import { connect } from 'node:http2';

let registerHandler = <T1,T2>(registerF: (handler: RequestHandler<T1, T2, void>) => void) =>
    (f: (args: T1) => () => T2) => () => registerF(x => f(x)());

let registerHandler0 = <T>(registerF: (handler: RequestHandler0<T, void>) => void) =>
    (f: () => T) => () => registerF(f);

let registerNotificationHandler = <T>(registerF: (handler: NotificationHandler<T>) => void) =>
    (f: (args: T) => () => void) => () => registerF(x => f(x)());

let registerNotificationHandler0 = (registerF: (handler: NotificationHandler0) => void) =>
    (f: () => void) => () => registerF(f);

export const onDefinition = (conn: Connection) => registerHandler(conn.onDefinition);

export const onCompletion = (conn: Connection) => registerHandler(conn.onCompletion);

export const onHover = (conn: Connection) => registerHandler(conn.onHover);

export const onSignatureHelp = (conn: Connection) => registerHandler(conn.onSignatureHelp);

export const onDocumentSymbol = (conn: Connection) => registerHandler(conn.onDocumentSymbol);

export const onWorkspaceSymbol = (conn: Connection) => registerHandler(conn.onWorkspaceSymbol);

export const onReferences = (conn: Connection) => registerHandler(conn.onReferences);

export const onCodeAction = (conn: Connection) => registerHandler(conn.onCodeAction);

export const onCodeLens = (conn: Connection) => registerHandler(conn.onCodeLens);

export const onFoldingRanges = (conn: Connection) => registerHandler(conn.onFoldingRanges);

export const onDocumentFormatting = (conn: Connection) => registerHandler(conn.onDocumentFormatting);

export const onDidChangeConfiguration = (conn: Connection) => registerNotificationHandler(conn.onDidChangeConfiguration);

export const publishDiagnostics = (conn: Connection) => (params: PublishDiagnosticsParams) => () => conn.sendDiagnostics(params);

export const applyEditImpl = (conn: Connection) => (edit: WorkspaceEdit) => () => conn.workspace.applyEdit(edit).then(x => x.applied);

export const sendDiagnosticsBegin = (conn: Connection) => () => conn.sendNotification(new NotificationType0('textDocument/diagnosticsBegin'));

export const sendDiagnosticsEnd = (conn: Connection) => () => conn.sendNotification(new NotificationType0('textDocument/diagnosticsEnd'));

export const onExecuteCommand = (conn: Connection) => registerHandler(conn.onExecuteCommand);

export const onDidChangeWatchedFiles = (conn: Connection) => registerNotificationHandler(conn.onDidChangeWatchedFiles);

export const onExit = (conn: Connection) => registerNotificationHandler0(conn.onExit);

export const onShutdown = (conn: Connection) => registerHandler0(conn.onShutdown);

