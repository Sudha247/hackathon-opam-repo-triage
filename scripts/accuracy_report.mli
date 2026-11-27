(** Compare an [evaluate-closed-prs --json] result with the previous one, and
    record it.

    {v accuracy-report eval.json --history history/ [--commit SHA] v}

    Reads history/latest.json (the previous evaluation, if any), prints a
    Markdown report, appends a line to history/history.jsonl and replaces
    history/latest.json with this evaluation.

    Two comparisons:

    - the totals (precision: suggested changes matching the author's fix;
      recall: the author's changes the suggestions anticipated). They depend on
      which PRs were merged recently, so they move from week to week;
    - the PRs evaluated both times (same initial commit): a suggestion that
      matched the author's fix last time and does not any more is a regression
      of the tool, whatever the sample.

    Exits with status 1 on a regression, or when precision drops by --max-drop
    points or more (with at least --min-scored suggestions), so that a scheduled
    job fails and GitHub notifies. Not when the two runs were scored differently
    (the "scoring" of the totals, since 27 November 2026): the report says that
    they are not comparable. *)

val main : unit -> unit
(** Run [accuracy-report] with the command line's arguments. *)
