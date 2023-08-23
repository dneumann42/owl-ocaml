val parse_json_body :
  string -> (Yojson.Safe.t -> ('a, 'b) result) -> 'a option
val parse_json_req :
  'a Dream.message -> (Yojson.Safe.t -> ('b, 'c) result) -> 'b option Lwt.t
val exec_query :
  Dream.request -> (Caqti_lwt.connection -> unit Lwt.t) -> unit Lwt.t
module Note :
  sig
    type create_input = { text : string; }
    type delete_input = { id : int; }
    type update_input = { id : int; text : string; }
    val create_note : Dream.client Dream.message -> Dream.response Lwt.t
    val delete_note : Dream.client Dream.message -> Dream.response Lwt.t
    val update_note : Dream.client Dream.message -> Dream.response Lwt.t
  end
