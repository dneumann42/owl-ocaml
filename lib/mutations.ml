module type DB = Caqti_lwt.CONNECTION
module T = Caqti_type

let exec query v (module Db : DB) =
  let%lwt unit_or_error = Db.exec query v in
  Caqti_lwt.or_fail unit_or_error

module Note = struct
  let create_note_mutation ~text =
    let query =
      let open Caqti_request.Infix in
      (T.(tup2 string string) ->. T.unit)
      "INSERT INTO note (text, created_by) VALUES ($1, $2);" 
    in (exec query)(text, "dustin")

  let delete_note_mutation ~id =
    let query =
      let open Caqti_request.Infix in
      (T.int ->. T.unit)
      "DELETE FROM note WHERE id = $1;"
    in (exec query)(id)

  let update_note_mutation ~id ~text =
    let query =
      let open Caqti_request.Infix in
      (T.(tup2 int string) ->. T.unit)
      {| UPDATE note 
         SET text = $2
         WHERE id = $1 |}
    in (exec query)(id, text)
end