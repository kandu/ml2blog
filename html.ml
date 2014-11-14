module StringMap = Map.Make(String)

let escape_query s=
  Str.global_substitute
    (Str.regexp "&")
    (fun c-> "&amp;")
    s

let escape s=
  Str.global_substitute
    (Str.regexp "[ ><&=+\x00-\x1f]")
    (function
      | " " -> "+"
      | c ->
        Printf.sprintf "%%%02X" (int_of_char (Str.matched_string c).[0]))
    s

let unescape s=
  Str.global_substitute
    (Str.regexp "+\\|%[0-9a-fA-F][0-9a-fA-F]")
    (function
      | "+" -> "="
      | c ->
        String.sub (c |> Str.matched_string) 1 2
        |> Printf.sprintf "0x%s"
        |> int_of_string |> char_of_int |> String.make 1)
    s

let encode m=
  StringMap.bindings m
  |> List.map (fun (k,v)-> escape k ^ "=" ^ escape v)
  |> String.concat "&"

let decode s=
  try
    List.fold_left
      (fun acc pair->
        let kv= Str.(split (regexp "=") pair) in
        StringMap.add (List.hd kv) (List.hd (List.tl kv)) acc)
      StringMap.empty
      Str.(split (regexp "&") s)
  with _ -> StringMap.empty

let head title=
{|<html>
<head>
    <meta charset="utf-8"/>
    <title>|} ^ title ^ {|</title>
    <link rel="stylesheet" href="/css/main.css" type="text/css"/>
    <link rel="stylesheet" href="/css/markdown.css" type="text/css"/>
    <link rel="stylesheet" href="/css/highlight.css" type="text/css"/>
</head>
<body>
<div id="bg0"><div id="bg1">
|}

let tail= {|</div></div></body></html>|}
