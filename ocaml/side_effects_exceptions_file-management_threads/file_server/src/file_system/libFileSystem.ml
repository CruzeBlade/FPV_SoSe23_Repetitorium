open LibCommon

type file_reference = string
type file_content = string
type read_request = file_reference
type write_request = file_reference * file_content

module type FileSystem = sig
  type t

  exception FileSystemException

  val init : unit -> t
  val read : t -> read_request -> file_content
  val write : t -> write_request -> t
  val delete : t -> file_reference -> t
end

module PlaceholderFileSystem : FileSystem = struct
  type t = unit

  exception FileSystemException

  let init () = ()
  let read fs filename = failwith "placeholder"
  let write fs (filename, content) = failwith "placeholder"
  let delete fs filename = failwith "placeholder"
end

(* TODO *)
module InMemoryFileSystem : FileSystem = struct
  type t = (file_reference * file_content) list

  exception FileSystemException

  let init () = []

  let rec read fs filename =
    match List.assoc_opt filename fs with
    | None -> raise FileSystemException
    | Some c -> c

  let rec write fs (filename, content) =
    match fs with
    | [] -> [ (filename, content) ]
    | (fn, fc) :: xs ->
        if fn = filename then (fn, content) :: xs
        else (fn, fc) :: write xs (filename, content)

  let rec delete fs filename =
    match fs with
    | [] -> []
    | (fn, fc) :: xs ->
        if fn = filename then xs else (fn, fc) :: delete xs filename
end

(* TODO *)
module OnDiskFileSystem : FileSystem = struct
  type t = unit

  exception FileSystemException

  let init () = ()

  let rec read _ filename =
    let filehandle =
      try open_in filename with _ -> raise FileSystemException
    in
    let rec impl acc =
      try
        let line = input_line filehandle in
        impl acc ^ line
      with End_of_file -> acc
    in
    try
      let res = impl "" in
      close_in filehandle;
      res
    with _ ->
      close_in filehandle;
      raise FileSystemException

  let rec write _ (filename, content) =
    let filehandle =
      try open_out filename with _ -> raise FileSystemException
    in
    try
      output_string filehandle content;
      close_out filehandle
    with _ ->
      close_out filehandle;
      raise FileSystemException

  let rec delete _ filename =
    try Sys.remove filename with _ -> raise FileSystemException
end
