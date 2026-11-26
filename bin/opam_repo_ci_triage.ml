(* Triage opam-repo-ci failures of an ocaml/opam-repository PR: one
   subcommand per command of Triage.Cli, each with the options it uses. *)

open Cmdliner
open Triage

(* The options; each command takes some of them (see [commands]). *)

let target =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"TARGET" ~doc:"commit sha or opam.ci.ocaml.org URL")

let pr =
  Arg.(
    value
    & opt (some int) None
    & info [ "pr" ] ~docv:"PR"
        ~doc:"opam-repository PR number (its head commit is looked up)")

let cache_dir =
  Arg.(
    value
    & opt string ".opam-ci-cache"
    & info [ "cache-dir" ] ~docv:"CACHE_DIR"
        ~doc:
          "where CI pages, logs and lookups are cached (default: \
           .opam-ci-cache)")

let refresh =
  Arg.(
    value & flag
    & info [ "refresh" ]
        ~doc:
          "fetch the commit page again, and the logs of its jobs when needed \
           (after a rebuild)")

let json = Arg.(value & flag & info [ "json" ] ~doc:"machine-readable output")
let all ~doc = Arg.(value & flag & info [ "all" ] ~doc)

let match_ =
  Arg.(
    value
    & opt (some string) None
    & info [ "match" ] ~docv:"MATCH"
        ~doc:
          "only the jobs whose CI name (as in the job's URL) matches this regex")

let category =
  Arg.(
    value & opt_all string []
    & info [ "category" ] ~docv:"CATEGORY"
        ~doc:
          "only these categories (default: everything except \
           network/cancelled/ignored/not-available)")

let max ~doc = Arg.(value & opt int 30 & info [ "max" ] ~docv:"MAX" ~doc)

let llm =
  Arg.(
    value & flag
    & info [ "llm" ]
        ~doc:
          "ask Claude about failures the tool cannot explain (needs \
           credentials; costs API tokens; responses are cached)")

let llm_max =
  Arg.(
    value & opt int 5
    & info [ "llm-max" ] ~docv:"LLM_MAX"
        ~doc:"at most this many LLM calls (distinct failures)")

let llm_model =
  Arg.(
    value
    & opt string Llm_model.default
    & info [ "llm-model" ] ~docv:"LLM_MODEL"
        ~doc:("the Claude model for --llm (default: " ^ Llm_model.default ^ ")"))

let llm_dry_run =
  Arg.(
    value & flag
    & info [ "llm-dry-run" ]
        ~doc:"print the LLM prompts instead of calling the API")

let post ~doc = Arg.(value & flag & info [ "post" ] ~doc)

let baseline =
  Arg.(
    value
    & opt (some string) None
    & info [ "baseline" ] ~docv:"BASELINE"
        ~doc:"baseline commit sha/URL (default: the PR of the previous version)")

(* The options of every command: what to analyse, and the cache. *)
let common command =
  Term.(
    const (fun target pr cache_dir refresh ->
        { (Cli.default command) with target; pr; cache_dir; refresh })
    $ target $ pr $ cache_dir $ refresh)

(* The options of the commands that suggest fixes. *)
let suggesting command =
  Term.(
    const
      (fun
        (a : Cli.args)
        match_
        max
        llm
        llm_max
        llm_model
        llm_dry_run
        :
        Cli.args
      -> { a with match_; max; llm; llm_max; llm_model; llm_dry_run })
    $ common command $ match_
    $ max ~doc:"distinct failures to analyse (default: 30)"
    $ llm $ llm_max $ llm_model $ llm_dry_run)

let command name doc term =
  Cmd.v (Cmd.info name ~doc) Term.(const Cli.main $ term)

let commands =
  [
    command "summary" "One record per CI job, and the failing ones by package."
      Term.(
        const (fun (a : Cli.args) json all -> { a with json; all })
        $ common Summary $ json
        $ all ~doc:"also show passed/ignored/not-available jobs");
    command "logs" "Excerpts of the logs of the failed jobs."
      Term.(
        const (fun (a : Cli.args) json match_ category max ->
            { a with json; match_; category; max })
        $ common Logs $ json $ match_ $ category
        $ max ~doc:"logs to fetch (default: 30)");
    command "compare"
      "Failing reverse dependencies against the CI run of the previous \
       version's PR."
      Term.(
        const (fun (a : Cli.args) json all baseline ->
            { a with json; all; baseline })
        $ common Compare $ json
        $ all ~doc:"also list the pre-existing failures in full"
        $ baseline);
    command "suggest" "Suggested fixes, with their evidence."
      Term.(
        const (fun (a : Cli.args) json -> { a with json })
        $ suggesting Suggest $ json);
    command "comment" "A draft PR comment, in Markdown."
      Term.(
        const (fun (a : Cli.args) post -> { a with post })
        $ suggesting Comment
        $ post ~doc:"post it on the PR with gh (default: print it)");
    command "review" "A GitHub pull-request review, as JSON."
      Term.(
        const (fun (a : Cli.args) post -> { a with post })
        $ suggesting Review
        $ post ~doc:"post it on the PR with gh (default: print it)");
  ]

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "opam-ci-triage"
             ~doc:"Triage opam-repo-ci failures of an ocaml/opam-repository PR.")
          commands))
