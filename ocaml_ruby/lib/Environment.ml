open Types

module State = struct
  type storage = (string, Types.value) Hashtbl.t

  let create () : storage = Hashtbl.create 20
  let get_variable (st : storage) (name : string) : value = Hashtbl.find st name

  let set_variable (st : storage) (name : string) (v : value) =
    let () = Hashtbl.replace st name v in
    st
end
