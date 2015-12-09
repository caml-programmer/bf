module Hashtbl = struct
    include Hashtbl

    let keys table =
      Hashtbl.fold (fun key _ acc -> key :: acc) table []
	      
    let union_to tables union_table =
      List.iter (Hashtbl.iter (fun key value -> Hashtbl.add union_table key value))
		tables

    let union tables =
      let union_table = create 32 in
      union_to tables union_table;
      union_table
	
  end
