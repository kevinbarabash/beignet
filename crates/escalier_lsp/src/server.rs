use derive_visitor::{DriveMut, VisitorMut};
use std::collections::HashMap;
use std::error::Error;
use std::time::{SystemTime, UNIX_EPOCH};

use lsp_server::{
    Connection, ErrorCode, ExtractError, Message, Notification, Request, RequestId, Response,
    ResponseError,
};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::request::{HoverRequest, SemanticTokensFullRequest};
use lsp_types::*;

use escalier_ast::types::Type;
use escalier_ast::values::{Expr, Pattern, Position, Program, SourceLocation, Statement, TypeAnn};
use escalier_interop::parse::parse_dts;
use escalier_parser::parse;

use crate::semantic_tokens::get_semantic_tokens;

pub struct LanguageServer {
    pub lib: String,
    pub file_cache: HashMap<Url, String>,
}

impl LanguageServer {
    pub fn main_loop(
        &mut self,
        connection: &Connection,
    ) -> Result<(), Box<dyn Error + Sync + Send>> {
        for msg in &connection.receiver {
            match msg {
                Message::Request(req) => {
                    self.handle_request(connection, req)?;
                }
                Message::Response(resp) => {
                    eprintln!("got response: {resp:?}");
                }
                Message::Notification(note) => {
                    self.handle_notification(note)?;
                }
            }
        }
        Ok(())
    }

    pub fn handle_request(
        &self,
        connection: &Connection,
        req: Request,
    ) -> Result<(), Box<dyn Error + Sync + Send>> {
        if connection.handle_shutdown(&req)? {
            return Ok(());
        }

        eprintln!("got request: {req:?}");
        match req.method.as_str() {
            "textDocument/hover" => {
                let (id, params) = cast_req::<HoverRequest>(req)?;

                eprintln!("Handling HoverRequest");

                // NOTE: This is slow so we'll want to do this once once
                // on startup and re-use the results.
                eprintln!("parsing .d.ts");
                let mut checker = match parse_dts(&self.lib) {
                    Ok(checker) => {
                        eprintln!("success");
                        checker
                    }
                    Err(_) => {
                        eprintln!("error");
                        panic!("parsing .d.ts file failed");
                    }
                };

                let start = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards");

                let input = self
                    .file_cache
                    .get(&params.text_document_position_params.text_document.uri)
                    .unwrap();

                // Update ParseError to implement Error + Sync + Send
                eprintln!("parsing input");
                let mut prog = parse(input).unwrap();

                // Update TypeError to implement Error + Sync + Send
                eprintln!("inferring types");
                escalier_infer::infer_prog(&mut prog, &mut checker).unwrap();

                let end = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards");
                let elapsed = end - start;
                eprintln!("request took {}ms", elapsed.as_millis());

                // TODO: create a From impl to convert from one Position to another.
                let cursor_loc = Position {
                    line: params.text_document_position_params.position.line,
                    column: params.text_document_position_params.position.character,
                };

                let message = match get_type_at_location(&mut prog, &cursor_loc) {
                    Some(t) => t.to_string(),
                    None => String::from("no type info"),
                };

                let result = Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(message)),
                    range: None,
                });
                let resp = Response {
                    id,
                    result: Some(serde_json::to_value(&result).unwrap()),
                    error: None,
                };
                connection.sender.send(Message::Response(resp))?;
            }
            "textDocument/semanticTokens/full" => {
                let (id, params) = cast_req::<SemanticTokensFullRequest>(req)?;
                let resp = self.handle_semantic_tokens(id, params);

                connection.sender.send(Message::Response(resp))?;
            }
            method => {
                eprintln!("Unhandled request method: {method}");
            }
        }

        Ok(())
    }

    pub fn handle_notification(
        &mut self,
        note: Notification,
    ) -> Result<(), Box<dyn Error + Sync + Send>> {
        eprintln!("got notification: {note:?}");
        match note.method.as_str() {
            "textDocument/didOpen" => {
                let params = cast_note::<DidOpenTextDocument>(note)?;
                let TextDocumentItem {
                    uri,
                    version: _,
                    text,
                    language_id: _,
                } = params.text_document;

                self.file_cache.insert(uri, text);
            }
            "textDocument/didChange" => {
                let params = cast_note::<DidChangeTextDocument>(note)?;
                let VersionedTextDocumentIdentifier { uri, version: _ } = params.text_document;

                for change in params.content_changes {
                    match change.range {
                        Some(_) => todo!(),
                        None => {
                            self.file_cache.insert(uri.to_owned(), change.text);
                        }
                    }
                }
            }
            method => {
                eprintln!("Unhandled notification method: {method}");
            }
        }

        Ok(())
    }

    fn handle_semantic_tokens(&self, id: RequestId, params: SemanticTokensParams) -> Response {
        // TODO: if it isn't in the cache yet, we should load it from disk
        // TODO: if we can't load it from disk then we should report an error
        let input = match self.file_cache.get(&params.text_document.uri) {
            Some(input) => input,
            None => {
                return Response {
                    id,
                    result: None,
                    error: Some(ResponseError {
                        code: ErrorCode::InternalError as i32,
                        message: String::from("Couldn't find file in cache"),
                        data: None,
                    }),
                }
            }
        };

        let mut prog = match parse(input) {
            Ok(prog) => prog,
            Err(_) => {
                return Response {
                    id,
                    result: None,
                    error: Some(ResponseError {
                        code: ErrorCode::ParseError as i32,
                        message: String::from("Failed to parse file"),
                        data: None,
                    }),
                }
            }
        };

        let result = Some(SemanticTokensPartialResult {
            data: get_semantic_tokens(&mut prog),
        });

        let value = match serde_json::to_value(&result) {
            Ok(value) => value,
            Err(_) => {
                return Response {
                    id,
                    result: None,
                    error: Some(ResponseError {
                        code: ErrorCode::InternalError as i32,
                        message: String::from("Failed to convert result to Value"),
                        data: None,
                    }),
                }
            }
        };

        Response {
            id,
            result: Some(value),
            error: None,
        }
    }
}

#[derive(VisitorMut)]
#[visitor(
    Expr(enter),
    Pattern(enter),
    Program(enter),
    Statement(enter),
    TypeAnn(enter)
)]
struct GetTypeVisitor {
    cursor_pos: Position,
    t: Option<Type>,
}

fn is_pos_in_source_loc(pos: &Position, src_loc: &SourceLocation) -> bool {
    let is_after_start = pos.line > src_loc.start.line
        || (pos.line == src_loc.start.line && pos.column >= src_loc.start.column);
    let is_before_end = pos.line < src_loc.end.line
        || (pos.line == src_loc.end.line && pos.column < src_loc.end.column);

    is_after_start && is_before_end
}

// TODO: use is_pos_in_source_loc to terminate certain branches of the visitor
impl GetTypeVisitor {
    fn enter_expr(&mut self, expr: &Expr) {
        if is_pos_in_source_loc(&self.cursor_pos, &expr.loc) {
            if let Some(t) = &expr.inferred_type {
                self.t = Some(t.to_owned())
            }
        }
    }

    fn enter_pattern(&mut self, pattern: &Pattern) {
        if is_pos_in_source_loc(&self.cursor_pos, &pattern.loc) {
            if let Some(t) = &pattern.inferred_type {
                self.t = Some(t.to_owned())
            }
        }
    }

    fn enter_program(&mut self, _program: &Program) {
        eprintln!("enter_program");
        // Do nothing b/c Program doesn't have an .inferred_type field
    }

    fn enter_statement(&mut self, _stmt: &Statement) {
        eprintln!("enter_statement");
        // Do nothing b/c Statement doesn't have an .inferred_type field (yet)
    }
    fn enter_type_ann(&mut self, type_ann: &TypeAnn) {
        if is_pos_in_source_loc(&self.cursor_pos, &type_ann.loc) {
            if let Some(t) = &type_ann.inferred_type {
                self.t = Some(t.to_owned())
            }
        }
    }
}

fn get_type_at_location(program: &mut Program, cursor_pos: &Position) -> Option<Type> {
    let mut visitor = GetTypeVisitor {
        cursor_pos: cursor_pos.to_owned(),
        t: None,
    };
    program.drive_mut(&mut visitor);
    visitor.t
}

fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_note<R>(note: Notification) -> Result<R::Params, ExtractError<Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    note.extract(R::METHOD)
}

#[cfg(test)]
mod tests {
    use crossbeam::channel::unbounded;
    use lsp_types::Position;
    use serde_json::*;
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_handle_notification_did_open() {
        let file_cache = HashMap::new();

        let mut server = LanguageServer {
            file_cache,
            lib: String::from(""),
        };

        let uri = Url::from_str("file://path/to/file.esc").unwrap();
        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.to_owned(),
                language_id: String::from("escalier"),
                version: 123,
                text: String::from("let a = 5;"),
            },
        };

        let note = Notification {
            method: String::from("textDocument/didOpen"),
            params: to_value(params).unwrap(),
        };

        server.handle_notification(note).unwrap();

        assert!(server.file_cache.contains_key(&uri));
        assert_eq!(server.file_cache.get(&uri).unwrap(), "let a = 5;");
    }

    #[test]
    fn test_handle_notification_did_change() {
        let uri = Url::from_str("file://path/to/file.esc").unwrap();
        let mut file_cache = HashMap::new();
        file_cache.insert(uri.to_owned(), String::from("let a = 5;"));

        let mut server = LanguageServer {
            file_cache,
            lib: String::from(""),
        };

        let params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.to_owned(),
                version: 456,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None, // deprecated
                text: String::from("let a = 10;"),
            }],
        };

        let note = Notification {
            method: String::from("textDocument/didChange"),
            params: to_value(params).unwrap(),
        };

        server.handle_notification(note).unwrap();

        assert!(server.file_cache.contains_key(&uri));
        assert_eq!(server.file_cache.get(&uri).unwrap(), "let a = 10;");
    }

    #[test]
    fn test_handle_hover_request() {
        let uri = Url::from_str("file://path/to/file.esc").unwrap();
        let mut file_cache = HashMap::new();
        file_cache.insert(uri.to_owned(), String::from("let a = 5;"));

        let server = LanguageServer {
            file_cache,
            lib: String::from(""),
        };

        let params = HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position {
                    line: 0,
                    character: 4,
                },
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        };

        let req = Request {
            id: RequestId::from(3),
            method: String::from("textDocument/hover"),
            params: to_value(params).unwrap(),
        };

        let (writer_sender, writer_receiver) = unbounded();
        let (_, reader_receiver) = unbounded();

        let connection = Connection {
            sender: writer_sender,
            receiver: reader_receiver,
        };

        server.handle_request(&connection, req).unwrap();

        let msg: Message = writer_receiver.recv().unwrap();

        let mut map = Map::new();
        map.insert(String::from("contents"), Value::String(String::from("5")));

        insta::assert_snapshot!(format!("{msg:#?}"), @r###"
        Response(
            Response {
                id: RequestId(
                    I32(
                        3,
                    ),
                ),
                result: Some(
                    Object {
                        "contents": String("5"),
                    },
                ),
                error: None,
            },
        )
        "###);
    }
}
