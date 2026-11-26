(* Running commands, capturing their output. *)

type result = { code : int; stdout : string; stderr : string }

(* A command that exits before reading its input must not kill the tool:
   writing to it then fails with EPIPE, which [real_run] handles. *)
let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

exception Timeout of float

let which prog =
  match Bos.OS.Cmd.find_tool (Bos.Cmd.v prog) with
  | Ok (Some p) -> Some (Fpath.to_string p)
  | _ -> None

let env_with extra =
  let base = Array.to_list (Unix.environment ()) in
  let keys = List.map fst extra in
  let base =
    List.filter
      (fun kv ->
        match String.index_opt kv '=' with
        | Some i -> not (List.mem (String.sub kv 0 i) keys)
        | None -> true)
      base
  in
  Array.of_list (base @ List.map (fun (k, v) -> k ^ "=" ^ v) extra)

(* The default implementation; tests replace [run_hook]. *)
let real_run ?cwd ?(env = []) ?input ?timeout args =
  let prog = List.hd args in
  let in_r, in_w = Unix.pipe ~cloexec:true () in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let err_r, err_w = Unix.pipe ~cloexec:true () in
  let envv = env_with env in
  let pid =
    let spawn () =
      Unix.create_process_env prog (Array.of_list args) envv in_r out_w err_w
    in
    match cwd with
    | None -> (
        try spawn ()
        with e ->
          List.iter Unix.close [ in_r; in_w; out_r; out_w; err_r; err_w ];
          raise e)
    | Some d -> (
        (* create_process has no cwd: go through sh. *)
        let script = "cd \"$1\" && shift && exec \"$@\"" in
        try
          Unix.create_process_env "/bin/sh"
            (Array.of_list ([ "/bin/sh"; "-c"; script; "sh"; d ] @ args))
            envv in_r out_w err_w
        with e ->
          List.iter Unix.close [ in_r; in_w; out_r; out_w; err_r; err_w ];
          raise e)
  in
  Unix.close in_r;
  Unix.close out_w;
  Unix.close err_w;
  let input = Option.value ~default:"" input in
  let out = Buffer.create 4096 and err = Buffer.create 1024 in
  let chunk = Bytes.create 65536 in
  let deadline = Option.map (fun t -> Unix.gettimeofday () +. t) timeout in
  (* Feed [input] from [pos] and read both outputs until they are closed, or
     until the deadline. Returns whether the deadline passed, and the
     descriptors still open. *)
  let rec loop opened pos w_open =
    let left = Option.map (fun d -> d -. Unix.gettimeofday ()) deadline in
    match left with
    | _ when opened = [] -> (false, opened, w_open)
    | Some l when l <= 0. -> (true, opened, w_open)
    | _ ->
        let ws = if w_open then [ in_w ] else [] in
        let r, w, _ =
          try Unix.select opened ws [] (Option.value ~default:(-1.) left)
          with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
        in
        let opened =
          List.fold_left
            (fun opened fd ->
              let n =
                try Unix.read fd chunk 0 (Bytes.length chunk)
                with Unix.Unix_error _ -> 0
              in
              if n = 0 then (
                Unix.close fd;
                List.filter (fun x -> x != fd) opened)
              else (
                Buffer.add_subbytes (if fd == out_r then out else err) chunk 0 n;
                opened))
            opened r
        in
        if w = [] then loop opened pos w_open
        else
          (* A command that exits without reading its input: EPIPE, the rest
             of the input is dropped. *)
          let n =
            try
              Unix.single_write_substring in_w input pos
                (min 65536 (String.length input - pos))
            with Unix.Unix_error _ -> String.length input - pos
          in
          let pos = pos + n in
          let w_open = pos < String.length input in
          if not w_open then Unix.close in_w;
          loop opened pos w_open
  in
  if input = "" then Unix.close in_w;
  let timed_out, opened, w_open = loop [ out_r; err_r ] 0 (input <> "") in
  if w_open then Unix.close in_w;
  List.iter Unix.close opened;
  if timed_out then begin
    (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (Unix.waitpid [] pid);
    raise (Timeout (Option.value ~default:0. timeout))
  end;
  let _, st = Unix.waitpid [] pid in
  let code =
    match st with
    | Unix.WEXITED c -> c
    | Unix.WSIGNALED s -> -s
    | Unix.WSTOPPED s -> -s
  in
  { code; stdout = Buffer.contents out; stderr = Buffer.contents err }

let run_hook = ref real_run

let run ?cwd ?env ?input ?timeout args =
  !run_hook ?cwd ?env ?input ?timeout args
