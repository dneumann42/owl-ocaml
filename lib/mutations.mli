module type DB = Caqti_lwt.CONNECTION

module T = Caqti_type

val exec :
  ('a, unit, [< `Zero ]) Caqti_request.t ->
  'a ->
  (module DB) ->
  unit Lwt.t

module Note : sig
  val create_note_mutation :
    text:string -> (module DB) -> unit Lwt.t

  val delete_note_mutation :
    id:int -> (module DB) -> unit Lwt.t

  val update_note_mutation :
    id:int -> text:string -> (module DB) -> unit Lwt.t
end
