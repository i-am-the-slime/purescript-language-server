import { TextDocuments, TextDocumentChangeEvent } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';

export const getDocuments = (documents: TextDocuments<TextDocument>) => () => documents.all();

export const getDocumentImpl = (just: (_:any) => any, nothing: any, documents: TextDocuments<TextDocument>, uri: string) => { 
    const doc = documents.get(uri)
    return doc ? just(doc) : nothing
};

export const onDidSaveDocument = (documents: TextDocuments<TextDocument>) => (f: (e: TextDocumentChangeEvent<TextDocument>) => () => void) => () => documents.onDidSave(p => f(p)());

export const onDidOpenDocument = (documents: TextDocuments<TextDocument>) => (f: (e: TextDocumentChangeEvent<TextDocument>) => () => void) => () => documents.onDidOpen(p => f(p)());

export const onDidCloseDocument = (documents: TextDocuments<TextDocument>) => (f: (e: TextDocumentChangeEvent<TextDocument>) => () => void) => () => documents.onDidClose(p => f(p)());

export const onDidChangeContent = (documents: TextDocuments<TextDocument>) => (f: (e: TextDocumentChangeEvent<TextDocument>) => () => void) => () => documents.onDidChangeContent(p => f(p)());
