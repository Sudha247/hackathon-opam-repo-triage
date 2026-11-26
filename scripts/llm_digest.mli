(** Group the LLM diagnoses of the triage page by error, to find the recurring
    ones worth turning into rules.

    {v
    llm-digest                          # the page's data.json, report on stdout
    llm-digest llm-eval/*.json          # [suggest --json] outputs instead
    llm-digest --issue OWNER/REPO       # also create or update the GitHub issue
    v}

    Each LLM answer carries the log line that best names the error
    ([log_error]). Answers are grouped by that line with paths, numbers and
    identifiers masked, so that [Unbound value Conv.output] and
    [Unbound value Conv.dimension] fall in the same group; a group seen in
    several PRs is a candidate for a rule. A rule then replaces the LLM call for
    that error: it is free, deterministic and tested, and the group disappears
    from the next digest.

    With --issue, the report becomes the body of one open issue (titled by
    --title; created if needed, with [gh]; needs a token that can write issues).
*)

val main : unit -> unit
(** Run [llm-digest] with the command line's arguments. *)

val report :
  ?page:string -> (Triage.Json.t * Triage.Suggestion.t) list -> int -> string
(** [report ~page answers min_prs]: the digest (GitHub Markdown) of the LLM
    answers, each with the number of its PR; the groups seen in at least
    [min_prs] PRs are the candidates for a rule. An answer repeated on the
    failures that share its error counts once per PR. [page]: the triage page,
    for links. Everything in an answer is untrusted (the model quotes CI logs
    and opam files): prose goes through {!Triage.Md.md_text}, log lines and
    changes are code spans. *)

val masked : string -> string
(** The group key of an error line: paths, numbers, identifiers and quoted names
    masked. *)
