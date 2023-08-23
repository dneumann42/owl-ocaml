module Note :
  sig
    type t = { id : int; text : string; created_by : string; }
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end
