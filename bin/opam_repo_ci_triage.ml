(* Triage opam-repo-ci failures of an ocaml/opam-repository PR. *)

open Cmdliner

let cmd =
  let open Arg in
  let command =
    required
    & pos 0
        (some
           (enum
              (List.map
                 (fun c -> (c, c))
                 [
                   "summary"; "logs"; "compare"; "suggest"; "comment"; "review";
                 ])))
        None
    & info [] ~docv:"COMMAND"
        ~doc:"summary, logs, compare, suggest, comment or review."
  in
  let target =
    value
    & pos 1 (some string) None
    & info [] ~docv:"TARGET" ~doc:"commit sha or opam.ci.ocaml.org URL"
  in
  let pr =
    value
    & opt (some int) None
    & info [ "pr" ] ~docv:"PR"
        ~doc:"opam-repository PR number (its head commit is looked up)"
  in
  let json = value & flag & info [ "json" ] ~doc:"machine-readable output" in
  let all =
    value & flag
    & info [ "all" ] ~doc:"summary: also show passed/ignored/not-available jobs"
  in
  let match_ =
    value
    & opt (some string) None
    & info [ "match" ] ~docv:"MATCH"
        ~doc:
          "logs, suggest: only the jobs whose CI name (as in the job's URL) \
           matches this regex"
  in
  let category =
    value & opt_all string []
    & info [ "category" ] ~docv:"CATEGORY"
        ~doc:
          "logs: only these categories (default: everything except \
           network/cancelled/ignored/not-available)"
  in
  let max =
    value & opt int 30
    & info [ "max" ] ~docv:"MAX"
        ~doc:
          "logs: logs to fetch; suggest/comment: distinct failures to analyse \
           (default: 30)"
  in
  let llm =
    value & flag
    & info [ "llm" ]
        ~doc:
          "suggest/comment: ask Claude about failures the tool cannot explain \
           (needs credentials; costs API tokens; responses are cached)"
  in
  let llm_max =
    value & opt int 5
    & info [ "llm-max" ] ~docv:"LLM_MAX"
        ~doc:"at most this many LLM calls (distinct failures)"
  in
  let llm_model =
    value
    & opt string Triage.Llm_model.default
    & info [ "llm-model" ] ~docv:"LLM_MODEL"
        ~doc:
          ("the Claude model for --llm (default: " ^ Triage.Llm_model.default
         ^ ")")
  in
  let llm_dry_run =
    value & flag
    & info [ "llm-dry-run" ]
        ~doc:"print the LLM prompts instead of calling the API"
  in
  let post =
    value & flag
    & info [ "post" ]
        ~doc:"comment/review: post it on the PR with gh (default: print it)"
  in
  let baseline =
    value
    & opt (some string) None
    & info [ "baseline" ] ~docv:"BASELINE"
        ~doc:
          "compare: baseline commit sha/URL (default: the PR of the previous \
           version)"
  in
  let cache_dir =
    value
    & opt string ".opam-ci-cache"
    & info [ "cache-dir" ] ~docv:"CACHE_DIR"
        ~doc:
          "where CI pages, logs and lookups are cached (default: \
           .opam-ci-cache)"
  in
  let refresh =
    value & flag
    & info [ "refresh" ]
        ~doc:
          "fetch the commit page again, and the logs of its jobs when needed \
           (after a rebuild)"
  in
  let run command target pr json all match_ category max llm llm_max llm_model
      llm_dry_run post baseline cache_dir refresh =
    Triage.Cli.main
      {
        command;
        target;
        pr;
        json;
        all;
        match_;
        category;
        max;
        llm;
        llm_max;
        llm_model;
        llm_dry_run;
        post;
        baseline;
        cache_dir;
        refresh;
      }
  in
  Cmd.v
    (Cmd.info "opam-ci-triage"
       ~doc:"Triage opam-repo-ci failures of an ocaml/opam-repository PR.")
    Term.(
      const run $ command $ target $ pr $ json $ all $ match_ $ category $ max
      $ llm $ llm_max $ llm_model $ llm_dry_run $ post $ baseline $ cache_dir
      $ refresh)

let () = exit (Cmd.eval cmd)
