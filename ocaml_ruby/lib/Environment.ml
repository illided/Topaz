open Types
open Base

module State = struct
  type storage = (string, value, String.comparator_witness) Map.t list

  let create: storage = [Map.empty (module String)]

  let add_state ~global ~local = local @ global

  let rec get_variable (st : storage) (name : string) : value = 
    match st with
    | [] -> failwith ("Undefined variable " ^ name)
    | table::tail ->      
        match Map.find table name with
        | Some (v) -> v
        | None -> get_variable tail name

  let set_variable (st : storage) (name : string) (v : value) =
    match st with
    | [] -> failwith "Can't set in empty storage"
    | local::tail -> 
      let new_local = Map.set local ~key:name ~data:v in
      new_local::tail
end
