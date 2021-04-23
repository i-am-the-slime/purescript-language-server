"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.onDidChangeContent = exports.onDidCloseDocument = exports.onDidOpenDocument = exports.onDidSaveDocument = exports.getDocumentImpl = exports.getDocuments = void 0;
const getDocuments = (documents) => () => documents.all();
exports.getDocuments = getDocuments;
const getDocumentImpl = (just, nothing, documents, uri) => {
    const doc = documents.get(uri);
    return doc ? just(doc) : nothing;
};
exports.getDocumentImpl = getDocumentImpl;
const onDidSaveDocument = (documents) => (f) => () => documents.onDidSave(p => f(p)());
exports.onDidSaveDocument = onDidSaveDocument;
const onDidOpenDocument = (documents) => (f) => () => documents.onDidOpen(p => f(p)());
exports.onDidOpenDocument = onDidOpenDocument;
const onDidCloseDocument = (documents) => (f) => () => documents.onDidClose(p => f(p)());
exports.onDidCloseDocument = onDidCloseDocument;
const onDidChangeContent = (documents) => (f) => () => documents.onDidChangeContent(p => f(p)());
exports.onDidChangeContent = onDidChangeContent;
