module type IntListDataBase = sig
  val store_int_list : string -> int list -> unit
  val read_int_list : string -> int list
  val remove : string -> unit
  val concat : string -> string -> string -> unit
end

module DiskDataBase : IntListDataBase = struct
  (* Use List.iter next time*)
  let store_int_list filename list =
    let filehandle = open_out filename in
    let rec impl = function
      | [] -> ()
      | x :: xs ->
          output_string filehandle (string_of_int x ^ "\n");
          impl xs
    in
    try
      impl list;
      close_out filehandle
    with ex ->
      close_out filehandle;
      raise ex

  let read_int_list filename =
    let filehandle = open_in filename in
    let rec impl acc =
      try
        let line = input_line filehandle in
        impl (int_of_string (String.trim line) :: acc)
      with End_of_file -> List.rev acc
    in
    try
      let res = impl [] in
      close_in filehandle;
      res
    with e ->
      close_in filehandle;
      raise e

  let remove filename = Sys.remove filename

  let concat in_file1 in_file2 out_file =
    let f1h = open_in in_file1 in
    let f2h = open_in in_file2 in
    let f3h = open_out out_file in
    let rec impl fh =
      try
        output_string f3h (input_line fh ^ "\n");
        impl fh
      with End_of_file -> ()
    in
    try
      impl f1h;
      impl f2h;
      close_out f3h;
      close_in f1h;
      close_in f2h
    with ex ->
      close_out f3h;
      close_in f1h;
      close_in f2h;
      raise ex
end
