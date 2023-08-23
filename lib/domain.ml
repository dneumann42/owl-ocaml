module Note = struct
  type t = 
    { id : int
    ; text : string
    ; created_by : string }
    [@@deriving yojson]
end