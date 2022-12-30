use std::error::Error;
use std::fs;

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{
    request::HoverRequest, Hover, HoverContents, HoverProviderCapability, InitializeParams,
    MarkedString, ServerCapabilities,
};

use crochet_dts::parse_dts::parse_dts;
use crochet_parser::parse;

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

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                match cast::<HoverRequest>(req) {
                    Ok((id, params)) => {
                        eprintln!("got hoverRequest request #{id}: {params:?}");
                        let in_path = params
                            .text_document_position_params
                            .text_document
                            .uri
                            .to_file_path()
                            .unwrap(); // TODO: generate a real error if the path doesn't exist
                        eprintln!("reading {}", in_path.to_string_lossy());
                        let input = fs::read_to_string(in_path).unwrap();
                        eprintln!("parsing .d.ts");
                        let mut ctx = match parse_dts(&lib) {
                            Ok(ctx) => {
                                eprintln!("success");
                                ctx
                            }
                            Err(_) => {
                                eprintln!("error");
                                panic!("parsing .d.ts file failed");
                            }
                        };
                        eprintln!("parsed lib");
                        // Update ParseError to implement Error + Sync + Send
                        let mut prog = parse(&input).unwrap();
                        eprintln!("parsed input");
                        // Update TypeError to implement Error + Sync + Send
                        crochet_infer::infer_prog(&mut prog, &mut ctx).unwrap();
                        eprintln!("inferred prog");
                        // eprintln!("prog = {prog:#?}");

                        let result = Some(Hover {
                            contents: HoverContents::Scalar(MarkedString::String(String::from(
                                "Hello, world!",
                            ))),
                            range: None,
                        });
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                // ...
            }
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                eprintln!("got notification: {not:?}");
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
