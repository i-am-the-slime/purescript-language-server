import { TextDocument, Position, Range } from 'vscode-languageserver-textdocument';
import { DocumentUri } from 'vscode-languageserver-types';

export const getText = (document: TextDocument) => () => document.getText();

export const getTextAtRange = (document: TextDocument) => (range: Range) => () => document.getText(range);

export const getUri = (document: TextDocument) => document.uri;

export const getLanguageId = (document: TextDocument) => document.languageId;

export const getVersion = (document: TextDocument) => () => document.version;

export const getLineCount = (document: TextDocument) => () => document.lineCount;

export const offsetAtPosition = (document: TextDocument) => (pos: Position) => () => document.offsetAt(pos);

export const positionAtOffset = (document: TextDocument) => (offset: number) => () => document.positionAt(offset);

export const createTextDocument = (uri: DocumentUri) => (languageId: string) => (version: number) => (content: string) => () => TextDocument.create(uri, languageId, version, content)