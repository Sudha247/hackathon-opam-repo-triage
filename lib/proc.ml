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
  let pos = ref 0 in
  let w_open = ref true in
  if input = "" then (
    Unix.close in_w;
    w_open := false);
  let opened = ref [ out_r; err_r ] in
  let timed_out = ref false in
  (try
     while !opened <> [] do
       let left =
         match deadline with Some d -> d -. Unix.gettimeofday () | None -> -1.
       in
       if deadline <> None && left <= 0. then raise Exit;
       let ws = if !w_open then [ in_w ] else [] in
       let r, w, _ =
         try Unix.select !opened ws [] (if deadline = None then -1. else left)
         with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
       in
       List.iter
         (fun fd ->
           let n =
             try Unix.read fd chunk 0 (Bytes.length chunk)
             with Unix.Unix_error _ -> 0
           in
           if n = 0 then (
             Unix.close fd;
             opened := List.filter (fun x -> x != fd) !opened)
           else Buffer.add_subbytes (if fd == out_r then out else err) chunk 0 n)
         r;
       if w <> [] then begin
         let n =
           try
             Unix.single_write_substring in_w input !pos
               (min 65536 (String.length input - !pos))
           with Unix.Unix_error _ -> String.length input - !pos
         in
         pos := !pos + n;
         if !pos >= String.length input then (
           Unix.close in_w;
           w_open := false)
       end
     done
   with Exit -> timed_out := true);
  if !w_open then Unix.close in_w;
  List.iter Unix.close !opened;
  if !timed_out then begin
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
