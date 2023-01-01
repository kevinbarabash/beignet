use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::HoverParams;
use lsp_types::{
    notification::{DidChangeTextDocument, DidOpenTextDocument},
    request::HoverRequest,
    Hover, HoverContents, HoverProviderCapability, InitializeParams, MarkedString,
    ServerCapabilities, TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    VersionedTextDocumentIdentifier,
};

use crochet_ast::types::Type;
use crochet_ast::values::{Position, Program, SourceLocation};
use crochet_dts::parse_dts::parse_dts;
use crochet_parser::parse;

mod visitor;

use visitor::Visitor;

static LIB_ES5_D_TS: &str = "../../../node_modules/typescript/lib/lib.es5.d.ts";

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    })
    .unwrap();

    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
    // We use `dyn Error` here b/c we're mixing different APIs when generate
    // different error structs
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let lib = fs::read_to_string(LIB_ES5_D_TS).unwrap();
    let _params: InitializeParams = serde_json::from_value(params).unwrap();

    let mut file_cache: HashMap<Url, String> = HashMap::new();

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                match req.method.as_str() {
                    "textDocument/hover" => {
                        let (id, params) = cast_req::<HoverRequest>(req)?;
                        handle_hover_request(&connection, &lib, &file_cache, id, params)?;
                        continue;
                    }
                    _ => {
                        // log unrecognized method
                        todo!()
                    }
                }
            }
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(note) => {
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

                        file_cache.insert(uri, text);

                        continue;
                    }
                    "textDocument/didChange" => {
                        let params = cast_note::<DidChangeTextDocument>(note)?;
                        let VersionedTextDocumentIdentifier { uri, version: _ } =
                            params.text_document;

                        for change in params.content_changes {
                            match change.range {
                                Some(_) => todo!(),
                                None => {
                                    file_cache.insert(uri.to_owned(), change.text);
                                }
                            }
                        }

                        continue;
                    }
                    _ => {
                        // log unrecognized method
                        todo!()
                    }
                }
            }
        }
    }
    Ok(())
}

fn handle_hover_request(
    connection: &Connection,
    lib: &str,
    file_cache: &HashMap<Url, String>,
    id: RequestId,
    params: HoverParams,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("Handling HoverRequest");

    // NOTE: This is slow so we'll want to do this once once
    // on startup and re-use the results.
    eprintln!("parsing .d.ts");
    let mut ctx = match parse_dts(lib) {
        Ok(ctx) => {
            eprintln!("success");
            ctx
        }
        Err(_) => {
            eprintln!("error");
            panic!("parsing .d.ts file failed");
        }
    };

    let start = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");

    let input = file_cache
        .get(&params.text_document_position_params.text_document.uri)
        .unwrap();

    // Update ParseError to implement Error + Sync + Send
    eprintln!("parsing input");
    let mut prog = parse(input).unwrap();

    // Update TypeError to implement Error + Sync + Send
    eprintln!("inferring types");
    crochet_infer::infer_prog(&mut prog, &mut ctx).unwrap();

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

    Ok(())
}

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
impl Visitor for GetTypeVisitor {
    fn visit_program(&mut self, _: &crochet_ast::values::Program) {
        eprintln!("visit_program");
        // Do nothing b/c Program doesn't have an .inferred_type field
    }

    fn visit_statement(&mut self, _stmt: &crochet_ast::values::Statement) {
        eprintln!("visit_program");
        // Do nothing b/c Statement doesn't have an .inferred_type field (yet)
    }

    fn visit_pattern(&mut self, pat: &crochet_ast::values::Pattern) {
        if is_pos_in_source_loc(&self.cursor_pos, &pat.loc) {
            if let Some(t) = &pat.inferred_type {
                self.t = Some(t.to_owned())
            }
        }
    }

    fn visit_type_ann(&mut self, type_ann: &crochet_ast::values::TypeAnn) {
        if is_pos_in_source_loc(&self.cursor_pos, &type_ann.loc) {
            if let Some(t) = &type_ann.inferred_type {
                self.t = Some(t.to_owned())
            }
        }
    }

    fn visit_expr(&mut self, expr: &crochet_ast::values::Expr) {
        if is_pos_in_source_loc(&self.cursor_pos, &expr.loc) {
            if let Some(t) = &expr.inferred_type {
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
    visitor.visit(program);

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
