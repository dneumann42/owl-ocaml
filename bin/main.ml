open Owl

module type DB = Caqti_lwt.CONNECTION
module T = Caqti_type

type note = 
  { id : int
  ; text : string
  ; created_by : string }
  [@@deriving yojson]

let login req =
  match Dream.session_field req "username" with
  | None ->
    let%lwt () = Dream.invalidate_session req in
    let%lwt () = Dream.set_session_field req "username" "dustin" in
    Dream.empty `OK
  | Some username ->
    let%lwt () = Dream.set_session_field req "username" username in
    Dream.empty `OK

let get_notes_query =
  let query =
    let open Caqti_request.Infix in
    (T.unit ->* T.(tup3 int string string))
    "SELECT id, text, created_by FROM note;"
  in fun (module Db : DB) ->
    let%lwt notes_or_error = Db.collect_list query () in
    let%lwt notes_proto = Caqti_lwt.or_fail notes_or_error in
    Lwt.return (List.map (fun (id, text, created_by) -> { id = id; text = text; created_by = created_by }) notes_proto)

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
    ; Dream.post "/create-note" Controllers.Note.create_note
    ; Dream.post "/delete-note" Controllers.Note.delete_note
    ; Dream.post "/update-note" Controllers.Note.update_note ] ]