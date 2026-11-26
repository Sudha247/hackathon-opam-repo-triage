(* Untrusted text in GitHub Markdown (PR comments, reviews, the digest issue).

   Text from CI logs, lint messages, opam files, changelogs and LLM answers
   must not render links, images or HTML, notify people (@mentions) or create
   cross-reference events (#N, owner/repo#N, GH-N, issue URLs), and must not
   break out of the code blocks it is put in.

   - [md_text] is for prose: one line; Markdown and HTML specials are
     backslash-escaped; a word that GitHub could turn into a mention, a
     reference or a link (it has [@], [#], [://], [www.] or is [GH-N]) is put
     in a code span, where GitHub links nothing; `code` spans of the text
     itself stay code spans.
   - [md_code] is an inline code span, [md_fence] a code block, each fenced by
     more backticks than any run in the text.

   GitHub's mention, reference and autolink filters skip text in [code],
   [pre] and [a] elements, so a code span keeps the token readable and
   copyable (unlike a zero-width joiner) and links nothing. *)

let special = {|([\\`*_\[\]<>|&])|}
let linkish = {|[@#]|://|\bwww\.|\bgh-\d|}
let flat s = Py.strip (Rx.sub {|\s+|} " " s)

let longest_run s =
  List.fold_left (fun a r -> max a (String.length r)) 0 (Rx.findall "`+" s)

let md_code s =
  let s = flat s in
  if s = "" then ""
  else
    let fence = String.make (1 + longest_run s) '`' in
    let pad =
      if s.[0] = '`' || s.[String.length s - 1] = '`' then " " else ""
    in
    fence ^ pad ^ s ^ pad ^ fence

let md_fence ?(lang = "") text =
  let fence = String.make (max 3 (1 + longest_run text)) '`' in
  fence ^ lang ^ "\n" ^ text ^ "\n" ^ fence

let escape s = Rx.sub special {|\\\1|} s

let word w =
  if not (Rx.test ~flags:[ Rx.I ] linkish w) then escape w
  else
    (* Punctuation around the token stays outside the code span: `#123`. *)
    match Rx.fullmatch {|([(\["']*)(.*?)([.,;:!?)\]"']*)|} w with
    | Some m ->
        let lead = Rx.group m 1
        and core = Rx.group m 2
        and trail = Rx.group m 3 in
        if core <> "" then escape lead ^ md_code core ^ escape trail
        else md_code w
    | None -> md_code w

let words s = Rx.subf {|\S+|} (fun m -> word (Rx.group0 m)) s

let join pieces =
  let b = Buffer.create 256 in
  List.iter
    (fun p ->
      (* Two code spans side by side would merge into other backtick runs,
         and their content could be parsed as Markdown. *)
      let n = Buffer.length b in
      if n > 0 && Buffer.nth b (n - 1) = '`' && p <> "" && p.[0] = '`' then
        Buffer.add_char b ' ';
      Buffer.add_string b p)
    pieces;
  Buffer.contents b

(* Python's (?<!`)(`+)(?!`)(.+?)(?<!`)\1(?!`): code spans of the text,
   delimited by backtick runs of the same length. *)
let spans s =
  let n = String.length s in
  let run_at i =
    let j = ref i in
    while !j < n && s.[!j] = '`' do
      incr j
    done;
    !j - i
  in
  let rec scan i acc =
    if i >= n then List.rev acc
    else if s.[i] = '`' && (i = 0 || s.[i - 1] <> '`') then begin
      let k = run_at i in
      (* The closing run: the first later run of exactly k backticks, after at least one character. *)
      let rec close j =
        if j >= n then None
        else if s.[j] = '`' && s.[j - 1] <> '`' then
          let r = run_at j in
          if r = k && j > i + k then Some j else close (j + r)
        else close (j + 1)
      in
      match close (i + k + 1) with
      | Some j when j - (i + k) >= 1 ->
          scan (j + k) ((i, j + k, String.sub s (i + k) (j - i - k)) :: acc)
      | _ -> scan (i + k) acc
    end
    else scan (i + 1) acc
  in
  scan 0 []

let md_text ?limit s =
  let s = flat s in
  let s =
    match limit with
    | Some l when l > 0 && Py.length s > l ->
        Py.rstrip (Py.prefix s l) ^ "\xe2\x80\xa6"
    | _ -> s
  in
  let pieces = ref [] and pos = ref 0 in
  List.iter
    (fun (a, e, inner) ->
      pieces := md_code inner :: words (String.sub s !pos (a - !pos)) :: !pieces;
      pos := e)
    (spans s);
  pieces := words (String.sub s !pos (String.length s - !pos)) :: !pieces;
  join (List.filter (fun p -> p <> "") (List.rev !pieces))

let sha_re = "[0-9a-f]{7,40}"
let variant_re = "[A-Za-z0-9._+~,-]+"
let ci_url_re = "https://opam\\.ci\\.ocaml\\.org/[A-Za-z0-9._~,+%/=-]+"

let ci_job_url base sha variant =
  if Rx.fullmatch sha_re sha <> None && Rx.fullmatch variant_re variant <> None
  then Some (base ^ sha ^ "/variant/" ^ variant)
  else None

let safe_ci_url url =
  match url with
  | Some u when Rx.fullmatch ci_url_re u <> None -> Some u
  | _ -> None
