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
  Lwt.return @@ parse_json_body body json_parser

let exec_query req query =
  let%lwt () = Dream.sql req query in
  Lwt.return_unit;

module Note = struct
  open Mutations.Note

  type create_input =
    { text : string }
    [@@deriving yojson]

  type delete_input =
    { id : int }
    [@@deriving yojson]

  type update_input =
    { id : int
    ; text : string }
    [@@deriving yojson]

  let create_note req =
    match%lwt parse_json_req req create_input_of_yojson with
    | Some i ->
      let%lwt _ = exec_query req @@ create_note_mutation ~text:i.text in
      Dream.empty `OK
    | None -> 
      Dream.empty `Bad_Request

  let update_note req =
    match%lwt parse_json_req req update_input_of_yojson with
    | Some i ->
      let%lwt _ = exec_query req @@ update_note_mutation ~id:i.id ~text:i.text in
      Dream.empty `OK
    | None ->
      Dream.empty `Bad_Request

  let delete_note req =
    match%lwt parse_json_req req delete_input_of_yojson with
    | Some i ->
      let%lwt _ = exec_query req @@ delete_note_mutation ~id:i.id in
      Dream.empty `OK
    | None ->
      Dream.empty `Bad_Request
end