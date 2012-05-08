module Make (T : sig type t end) = struct
  module M = Map.Make (String)

  type key = M.key
  type value = T.t

  type t = value M.t

  let empty : t = M.empty
  let add : key -> value -> t -> t = M.add
  let find : key -> t -> value = M.find
end

include Map.Make (String)
