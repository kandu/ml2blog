open Cmdliner
open GcFile
open Blog
open Printf

let write_to file content=
  Unix.write file.fd content 0 (String.length content) |> ignore

let filter_suffix suffix name= not (Filename.check_suffix name suffix)
let filter_set set name= not (StringSet.mem name set)
let filter_ml= filter_suffix ".ml"

let listDir dir=
  let open Unix in
  let rec listDir dir =
    match try readdir dir with End_of_file -> "" with
    | "" -> []
    | item -> item :: listDir dir
  in
  listDir dir
    |> List.filter (function
      | "." | ".." -> false
      | _ -> true)

let copyFiles src dest filters=
  let dir= (Unix.opendir src) in
  let srcs= listDir dir in
  List.fold_left
    (fun src filter-> List.filter filter src)
    srcs
    filters
  |> List.iter (fun name->
    close_in (Unix.open_process_in
      (sprintf "cp -r %s %s"
        (Filename.concat src name) dest)));
  Unix.closedir dir

let reader path=
  let read_article path=
    if Toploop.run_script Format.std_formatter path [|path|]
    then (Obj.magic (Toploop.getvalue "article"):article)
    else failwith "syntax error"
  in
  let read_all dir=
    let read_posts posts dir=
      let path= Filename.concat dir "value.ml" in
      let id= Filename.basename dir in
      try StringMap.add id (read_article path) posts with 
      | Failure "syntax error"-> posts
    in
    let open Unix in
    let postDirs= listDir (opendir dir)
      |> List.map (Filename.concat dir)
      |> List.filter (fun path-> (stat path).st_kind = S_DIR)
    in List.fold_left
      read_posts
      StringMap.empty
      postDirs
  in
  let home= read_article (Filename.concat path "value.ml")
  and tabs= read_all (Filename.concat path "tab")
  and posts= read_all (Filename.concat path "post")
  and disqus_code=
    let disqus_file= open_in @@ String.concat Filename.dir_sep
      [path; "config"; "disqus.html"]
    in
    let code= BatPervasives.input_all disqus_file in
    close_in disqus_file;
    code
  in (
    { home;
      tabs;
      posts;},
    disqus_code
    )

let string_of_sentence= function
  | Raw s-> s
  | Text s-> sprintf {|<span class='text'>%s</span>|} (Html.escape s)
  | Image (uri,alt)-> sprintf {|<img src="%s" alt="%s"/>|} uri alt
  | Mkd s-> let (inChan, outChan)= Unix.open_process "markdown" in
    output_string outChan s;
    close_out outChan;
    let str= BatPervasives.input_all inChan in
    close_in inChan;
    str
  | Code (lang, code, lineno)->
    let options= match lineno with
    | None-> ""
    | Some lineno-> sprintf "-O linenos=table,linenostart=%d" lineno
    in
    let (inChan, outChan)= Unix.open_process
      (sprintf "pygmentize -l %s -f html %s" lang options)
    in
    output_string outChan code;
    close_out outChan;
    let str= BatPervasives.input_all inChan in
    close_in inChan;
    str
  | Dot s-> s

let string_of_paragraph paragraph=
  List.map
    string_of_sentence
    paragraph
  |> String.concat ""

let string_of_paragraphs paragraphs=
  List.map
    string_of_paragraph
    paragraphs
  |> String.concat "\n<div></div>\n"

let string_of_head article= (string_of_paragraphs article.head)
let string_of_content article= (string_of_paragraphs article.content)
let string_of_tail article= (string_of_paragraphs article.tail)

let string_of_postInfo ?(updatedDate=false) article=
  let date_update= function
    | None-> ""
    | Some date-> sprintf {|
        (updated on <span class="date">%s</span>)|}
        date
      and tags tagS=
    StringSet.elements tagS
    |> List.map
      (fun tag-> "<span>" ^ tag ^ "</span>")
    |> String.concat ""
  in
   sprintf {|
    [tags: <span class="tag">%s</span>]
    <span class="postInfo">
      <span>posted by </span><span id="author">%s</span>
      on <span class="date">%s</span>
      %s</span> |}
    (tags article.tags)
    article.author
    article.date
    (if updatedDate then
      date_update article.date_update
    else "")

let string_of_post article=
  sprintf {|
    <div><div class="title_post">%s</div>%s</div>
    <div class="clear"></div>
    <div class="underline"></div>
    %s %s %s|}
    article.title
    (string_of_postInfo ~updatedDate:true article)
    (string_of_head article)
    (string_of_content article)
    (string_of_tail article)

let writer path (site, disqus_code)=
  let mkdirs ()=
    begin
      (try Unix.mkdir path 0o777 with _-> ());
      (try Unix.mkdir (Filename.concat path "tab") 0o777 with _-> ());
      (try Unix.mkdir (Filename.concat path "post") 0o777 with _-> ());
      StringMap.iter
        (fun tab _->
          try Unix.mkdir Filename.(concat (concat path "tab") tab) 0o777
          with _-> ())
        site.tabs;
      StringMap.iter
        (fun post _->
          try Unix.mkdir Filename.(concat (concat path "post") post) 0o777
          with _-> ())
        site.posts;
    end
  and genHome ()=
      let file= of_fd
        Unix.(openfile
          (String.concat Filename.dir_sep [path; "index.html"])
          [O_WRONLY; O_CREAT; O_TRUNC] 0o777)
      and sortedPosts=
        StringMap.fold
          (fun id post l-> (int_of_string id,post)::l)
          site.posts
          []
        |> List.sort (fun (a,_) (b,_)-> -(compare a b))
      in
      let head= string_of_head site.home
      and tail= string_of_tail site.home in
      write_to file Html.head;
      write_to file head;
      List.map
        (fun (id, post)->
          sprintf
            {|<div class="postItem"><a href="%s">%s</a>%s</div><div class="clear"></div>|}
            (String.concat Filename.dir_sep ["post"; (string_of_int id); "value.html"])
            post.title
            (string_of_postInfo post))
        sortedPosts
        |> String.concat {|<div class="postItem_sep"></div>|}
        |> write_to file;
      write_to file tail;
      write_to file Html.tail
  and genPosts disqus_code=
    let genPost id post path=
      let file= of_fd
        Unix.(openfile path [O_WRONLY; O_CREAT; O_TRUNC] 0o777)
      and head= string_of_head post
      and content= string_of_post post
      and tail= string_of_tail post
      in
      write_to file Html.head;
      write_to file head;
      write_to file @@ sprintf
        {|<script>var disqus_identifier= '%s';</script>|} id;
      write_to file content;
      write_to file {|<div class="line_sep"></div>|};
      write_to file disqus_code;
      write_to file tail;
      write_to file Html.tail
    in
    StringMap.iter
      (fun id post-> genPost id post
        (String.concat Filename.dir_sep [path; "post"; id; "value.html"]))
      site.posts
  in
  mkdirs ();
  genHome ();
  genPosts disqus_code

module Argv= struct
  let args src dest=
    let genPath path=
      if path = "" then Unix.getcwd ()
      else
        if Filename.is_relative path
        then Filename.concat (Unix.getcwd ()) path
        else path
    in
    let src= genPath src in
    let dest= if dest = "" then src ^ "-out" else genPath dest in
    (src, dest)

  let src=
    let doc= "the directory where ml2blog runs on" in
    Arg.(value & pos 0 string "" & info [] ~docv:"SRC" ~doc)

  let out=
    let doc= "the directory where ml2blog generates the result in" in
    Arg.(value & pos 1 string "" & info [] ~docv:"OUT" ~doc)

  let args_t= Term.(pure args $ src $ out)

  let info=
    let doc = "a blog generator" in
    let man = [ `S "BUGS"; `P "issue bug reports to https://bitbucket.org/zandoye/ml2blog";] in
    Term.info "ml2blog" ~version:"0.1" ~doc ~man
end

let ()=
  match Term.eval Argv.(args_t, info) with
  | `Ok (src, dest)->
    let (site, disqus)= reader src in
    writer dest (site, disqus);
    (* copy files in each post dir *)
    StringMap.iter
      (fun id _->
        let src= String.concat Filename.dir_sep [src;"post";id]
        and dest= String.concat Filename.dir_sep [dest;"post";id] in
        copyFiles src dest [filter_ml])
      site.posts;
    (* copy files in each tab dir *)
    StringMap.iter
      (fun id _->
        let src= String.concat Filename.dir_sep [src;"tab";id]
        and dest= String.concat Filename.dir_sep [dest;"tab";id] in
        copyFiles src dest [filter_ml])
      site.tabs;
    (* copy files in the top dir *)
    copyFiles src dest
      [ filter_ml;
        filter_set (StringSet.of_list ["config";"post";"tab";])]
  | _-> exit 1

(* vim: tabstop=8:softtabstop=2:shiftwidth=2:textwidth=0:
*)
