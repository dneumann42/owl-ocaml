module type DB = Caqti_lwt.CONNECTION
module T = Caqti_type

type create_note_input = 
  { text : string }
  [@@deriving yojson]

type delete_note_input =
  { id : int }
  [@@deriving yojson]

type update_note_input =
  { id : int
  ; text : string }
  [@@deriving yojson]

type note = 
  { id : int
  ; text : string
  ; created_by : string }
  [@@deriving yojson]

let parse_json_body body (json_parser : Yojson.Safe.t -> 'a) =
  (try Some (body |> Yojson.Safe.from_string |> json_parser) with _ -> None)
  |> function
  | Some doc -> begin
      match doc with
      | Ok t -> Some t
      | Error _ -> None
    end
  | None -> None

let parse_json_req req (json_parser : Yojson.Safe.t -> 'a) =
  let%lwt body = Dream.body req in
  Lwt.return (parse_json_body body json_parser)

let user_id req = 
  let _ = req in
  Some "dustin"

let login req =
  match Dream.session_field req "username" with
  | None ->
    let%lwt () = Dream.invalidate_session req in
    let%lwt () = Dream.set_session_field req "username" "dustin" in
    Dream.empty `OK
  | Some username ->
    let%lwt () = Dream.set_session_field req "username" username in
    Dream.empty `OK

let add_note_mutation =
  let query =
    let open Caqti_request.Infix in
    (T.(tup2 string string) ->. T.unit)
    "INSERT INTO note (text, created_by) VALUES ($1, $2);" 
  in fun text (module Db : DB) ->
    let%lwt unit_or_error = Db.exec query text in
    Caqti_lwt.or_fail unit_or_error

let delete_note_mutation =
  let query =
    let open Caqti_request.Infix in
    (T.int ->. T.unit)
    "DELETE FROM note WHERE id = $1;"
  in fun id (module Db : DB) ->
    let%lwt unit_or_error = Db.exec query id in
    Caqti_lwt.or_fail unit_or_error

let update_note_mutation =
  let query =
    let open Caqti_request.Infix in
    (T.(tup2 int string) ->. T.unit)
    {| UPDATE note 
       SET text = $2
       WHERE id = $1 |}
  in fun vs (module Db : DB) ->
    let%lwt unit_or_error = Db.exec query vs in
    Caqti_lwt.or_fail unit_or_error

let get_notes_query =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->* T.(tup3 int string string))
    "SELECT id, text, created_by FROM note;"
  in fun (module Db : DB) ->
    let%lwt notes_or_error = Db.collect_list query () in
    let%lwt notes_proto = Caqti_lwt.or_fail notes_or_error in
    Lwt.return (List.map (fun (id, text, created_by) -> { id = id; text = text; created_by = created_by }) notes_proto)

let create_note req =
    let%lwt n = (parse_json_req req create_note_input_of_yojson) in
    match user_id req with 
    | None -> 
      Dream.empty `Bad_Request
    | Some id -> begin
      match n with
      | Some i -> begin
        if String.length i.text > 0 then
          let%lwt () = Dream.sql req (add_note_mutation (i.text, id)) in
          Dream.json {| "success" |}
        else 
          Dream.empty `Bad_Request
      end
      | None ->
        Dream.empty `OK
    end

let delete_note req =
  let%lwt i = (parse_json_req req delete_note_input_of_yojson) in
  match i with
  | Some i ->
    let%lwt () = Dream.sql req (delete_note_mutation i.id) in
    Dream.empty `OK
  | None -> 
    Dream.empty `Bad_Request

let update_note req =
  let%lwt i = (parse_json_req req update_note_input_of_yojson) in
  match i with
  | Some i ->
    let%lwt () = Dream.sql req (update_note_mutation (i.id, i.text)) in
    Dream.empty `OK
  | None ->
    Dream.empty `Bad_Request

let get_notes req =
  let%lwt notes = Dream.sql req get_notes_query in
  let notes = `List (List.map note_to_yojson notes) in
  let s = (Yojson.Safe.to_string notes) in
  Dream.json s

let cors_middleware handler req =
  let handlers =
    [ "Access-Control-Allow-Origin", "*" ]
  in
  let%lwt res = handler req in
  handlers
  |> List.map (fun (key, value) -> Dream.add_header res key value)
  |> ignore;
  Lwt.return res

let () =
  Dream.run 
  @@ cors_middleware
  @@ Dream.logger
  @@ Dream.sql_pool "sqlite3:db.sqlite"
  @@ Dream.sql_sessions
  @@ Dream.router 
  [ Dream.get "/"
    (fun _ -> Dream.html "<h1> hello </h1>")
  ; Dream.scope "/api" [] 
    [ Dream.get "/echo/:word" 
        (fun req -> Dream.html (Dream.param req "word"))
    ; Dream.get "/login" login
    ; Dream.get "/notes" get_notes
    ; Dream.post "/create-note" create_note
    ; Dream.post "/delete-note" delete_note
    ; Dream.post "/update-note" update_note ] ]