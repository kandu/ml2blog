module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type lang= string
type code= string
type lineno= int option
type uri= string

type sentence=
  | Raw of string
  | Text of string
  | Image of uri * string
  | Mkd of string
  | Code of lang * lineno * code
  | Dot of string

type paragraph= sentence list

type article= {
  title: string;
  author: string;
  date: string;
  date_update: string option;
  tags: StringSet.t;
  head: paragraph list;
  content: paragraph list;
  tail: paragraph list;
}

type site=
  { home: article;
    tabs: article StringMap.t;
    posts: article StringMap.t}

