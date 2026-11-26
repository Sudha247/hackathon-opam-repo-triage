(** Run [suggest --llm] on the PRs of the triage page that have failures no rule
    recognises, and save each result for grading.

    {v
    llm-eval --first 5        # try 5 PRs, look at the token counts
    llm-eval                  # the rest (PRs already done are skipped)
    llm-eval --dry-run        # list the PRs and the number of prompts, no API call
    v}

    Needs ANTHROPIC_API_KEY (see the README). Each distinct failure is one call
    to the model, at most --llm-max per PR; answers are cached in the tool's
    cache directory, so running again costs nothing. Results go to
    llm-eval/<PR>.json (and .err); a summary of the tokens used is printed after
    each PR and at the end. *)

val main : unit -> unit
(** Run [llm-eval] with the command line's arguments. *)
