open LibCommon
open LibFileSystem
open Event
open Thread

module type FileServer = sig
  type t

  exception FileNotFound

  val create_file_server : unit -> t
  val read : t -> read_request -> file_content
  val write : t -> write_request -> unit
  val delete : t -> file_reference -> unit
  val kill : t -> unit
end

(*module PlaceholderFileServer = struct
    type t = unit

    exception FileNotFound

    let create_file_server () = ()
    let read server req = failwith "TODO"
    let write server req = failwith "TODO"
    let delete server req = failwith "TODO"
    let kill server = failwith "TODO"
  end*)

(* TODO *)
module CreateFileServer (FS : FileSystem) : FileServer = struct
  type req =
    | Read of (file_reference * (file_content, exn) result channel)
    | Write of ((file_reference * file_content) * exn option channel)
    | Delete of (file_reference * exn option channel)
    | Kill

  type t = req channel * Thread.t

  exception FileNotFound

  let create_file_server () =
    let req_channel = new_channel () in
    let rec serve fs =
      match sync (receive req_channel) with
      | Read (fn, res) ->
          let ret = try Ok (FS.read fs fn) with e -> Error e in
          sync (send res ret);
          serve fs
      | Write (file, res) -> (
          try
            let fs = FS.write fs file in
            sync (send res None);
            serve fs
          with e ->
            sync (send res (Some e));
            serve fs)
      | Delete (fn, res) -> (
          try
            let fs = FS.delete fs fn in
            sync (send res None);
            serve fs
          with e -> sync (send res (Some e)))
      | Kill -> ()
    in
    let t = create serve (FS.init ()) in
    (req_channel, t)

  let read (server, _) req =
    let res = new_channel () in
    sync (send server (Read (req, res)));
    match sync (receive res) with Ok c -> c | Error ex -> raise ex

  let write (server, _) req =
    let res = new_channel () in
    sync (send server (Write (req, res)));
    match sync (receive res) with None -> () | Some ex -> raise ex

  let delete (server, _) req =
    let res = new_channel () in
    sync (send server (Delete (req, res)));
    match sync (receive res) with None -> () | Some ex -> raise ex

  let kill (server, t) =
    sync (send server Kill);
    Thread.join t
end

(* TODO *)
module InMemoryFileServer = CreateFileServer (InMemoryFileSystem)

(* TODO *)
module OnDiskFileServer = CreateFileServer (OnDiskFileSystem)
