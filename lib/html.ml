(* HTML: escaping and unescaping (the CI pages are parsed with lambdasoup). *)

let escape ?(quote = true) s =
  let b = Buffer.create (String.length s + 16) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' when quote -> Buffer.add_string b "&quot;"
      | '\'' when quote -> Buffer.add_string b "&#x27;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let utf8 u =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b
    (if Uchar.is_valid u then Uchar.of_int u else Uchar.rep);
  Buffer.contents b

let entities =
  [
    ("amp", "&");
    ("lt", "<");
    ("gt", ">");
    ("quot", "\"");
    ("apos", "'");
    ("nbsp", "\xc2\xa0");
    ("copy", "\xc2\xa9");
    ("reg", "\xc2\xae");
    ("hellip", "\xe2\x80\xa6");
    ("mdash", "\xe2\x80\x94");
    ("ndash", "\xe2\x80\x93");
    ("lsquo", "\xe2\x80\x98");
    ("rsquo", "\xe2\x80\x99");
    ("ldquo", "\xe2\x80\x9c");
    ("rdquo", "\xe2\x80\x9d");
    ("laquo", "\xc2\xab");
    ("raquo", "\xc2\xbb");
    ("times", "\xc3\x97");
    ("middot", "\xc2\xb7");
    ("bull", "\xe2\x80\xa2");
    ("rarr", "\xe2\x86\x92");
    ("larr", "\xe2\x86\x90");
    ("para", "\xc2\xb6");
    ("sect", "\xc2\xa7");
    ("deg", "\xc2\xb0");
    ("plusmn", "\xc2\xb1");
  ]

(* Windows-1252 code points that numeric references in 0x80-0x9f stand for. *)
let cp1252 = function
  | 0x80 -> 0x20AC
  | 0x82 -> 0x201A
  | 0x83 -> 0x0192
  | 0x84 -> 0x201E
  | 0x85 -> 0x2026
  | 0x86 -> 0x2020
  | 0x87 -> 0x2021
  | 0x88 -> 0x02C6
  | 0x89 -> 0x2030
  | 0x8A -> 0x0160
  | 0x8B -> 0x2039
  | 0x8C -> 0x0152
  | 0x8E -> 0x017D
  | 0x91 -> 0x2018
  | 0x92 -> 0x2019
  | 0x93 -> 0x201C
  | 0x94 -> 0x201D
  | 0x95 -> 0x2022
  | 0x96 -> 0x2013
  | 0x97 -> 0x2014
  | 0x98 -> 0x02DC
  | 0x99 -> 0x2122
  | 0x9A -> 0x0161
  | 0x9B -> 0x203A
  | 0x9C -> 0x0153
  | 0x9E -> 0x017E
  | 0x9F -> 0x0178
  | u -> u

let unescape s =
  if not (String.contains s '&') then s
  else
    Rx.subf {|&(#[0-9]+;?|#[xX][0-9a-fA-F]+;?|[A-Za-z][A-Za-z0-9]*;?)|}
      (fun m ->
        let r = Rx.group m 1 in
        let r' = Util.rstrip ~chars:";" r in
        if r.[0] = '#' then begin
          let n =
            if String.length r' > 1 && (r'.[1] = 'x' || r'.[1] = 'X') then
              int_of_string_opt ("0x" ^ String.sub r' 2 (String.length r' - 2))
            else int_of_string_opt (String.sub r' 1 (String.length r' - 1))
          in
          match n with
          | Some 0 -> "\xef\xbf\xbd"
          | Some n when n > 0x10FFFF || (n >= 0xD800 && n <= 0xDFFF) ->
              "\xef\xbf\xbd"
          | Some n -> utf8 (cp1252 n)
          | None -> Rx.group0 m
        end
        else
          match List.assoc_opt r' entities with
          | Some v -> v
          | None -> Rx.group0 m)
      s
