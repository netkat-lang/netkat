open Idds
open Base

module Pattern = struct
  type t = (Var.t * bool) list
    [@@deriving sexp, compare, hash]
end

module Action = struct
  module T = struct
    type t = (Var.t * bool) list
      [@@deriving sexp, compare, hash]
  end
  include T
  include Comparator.Make(T)
end

type row = Pattern.t * Set.M(Action).t

type table = row list

type t = (Pattern.t, Set.M(Action).t) Hashtbl.t


(** [intersect lst tbl] returns a forwarding table computed from all possible 
    intersections of rows in [lst] *)
let rec intersect (lst:table) (tbl:t) =
  let rec build_intersections to_be_intersected rules lst =
    match lst with
    | [] -> 
        let intersection = List.concat to_be_intersected in
        if List.contains_dup ~compare:(fun (v, b) (v', b') -> 
            Bool.to_int (not (Var.equal v v') || (Bool.equal b b'))
          ) intersection || (List.length to_be_intersected) = 0 then ()
        else
          let pattern = List.dedup_and_sort ~compare:(fun (v, _) (v', _) -> 
            match Var.closer_to_root v v' with
            | Left -> 1
            | Equal -> 0
            | Right -> -1
          ) intersection in
          Hashtbl.update tbl pattern ~f:(fun r ->
            match r with
            | None -> rules
            | Some s -> Set.union s rules
          )
    | (p,a)::t -> 
      build_intersections (p::to_be_intersected) (Set.union rules a) t;
      build_intersections to_be_intersected rules t;
  in build_intersections [] (Set.empty (module Action)) lst; 
  List.sort (Hashtbl.to_alist tbl) ~compare:(fun (l, _) (l', _) -> 
    compare (List.length l') (List.length l)
  )


let to_table (idd:Idd.t) = 
  let tbl = Hashtbl.create (module Pattern) in
  let rec to_tables (dd:Dd.t) (pattern:Pattern.t) (rule:Action.t) : unit =
    match dd with
    | True -> 
      let set = (Set.singleton (module Action) rule) in
      Hashtbl.update tbl pattern ~f:(fun r ->
        match r with
        | None -> set
        | Some s -> Set.union s set
      )
    | False -> ()
    | Branch { var; hi; lo } when Var.is_inp var -> 
      (to_tables hi ((var, true)::pattern) rule);
      (to_tables lo ((var, false)::pattern) rule)
    | Branch { var; hi; lo } (* var is output *) ->
      (to_tables hi pattern ((var, true)::rule));
      (to_tables lo pattern ((var, false)::rule))
    in
  to_tables (idd :> Dd.t) [] [];
  intersect (Hashtbl.to_alist tbl) tbl


let eval (tbl:table) (env:(Var.t -> bool)) : bool =
  let check_lst lst = List.for_all lst ~f:(fun (v, b) -> Bool.equal b (env v)) in
  let rec eval = function
    | (p, a)::t -> if check_lst p then (Set.exists a ~f:check_lst)
                                  else eval t
    | [] -> false
  in
  eval tbl

let rec to_expr (tbl:table) ~interp_test ~interp_act = 
  match tbl with
  | (p,a)::t -> 
    Kat.Optimize.union
      (Kat.Optimize.seq 
        (Kat.Ast.Assert (interp_test p))
        (Set.fold a ~init:(Kat.Ast.Assert (Kat.Ast.False)) ~f:(fun exp elm -> 
            Kat.Optimize.union exp (interp_act elm)
          )
        )
      )
      (to_expr t ~interp_test ~interp_act)
  | [] -> Kat.Ast.Assert (Kat.Ast.False)

(** [to_str_lst p s] is "x1[s]b1;x2[s]b2];...;xn[s]bn" if 
    [p]=\[(x1,b1);...;(xn,bn)\] *)
let to_str_lst p s = String.drop_suffix
  (List.fold_right ~f:(fun (v, b) str ->
      let str1 = Caml.Format.sprintf "x%d" (Var.index v) in
      let str2 = Caml.Format.sprintf (if b then ("1;%s") else "0;%s") str in
      str1 ^ s ^ str2
    )
    ~init:""
    p
  )
  1

(** [to_str_act act] is "s1;s2;...;sn" where [act]=\{a1,...,an\} and
    [si] is [to_str_lst ai "←"] *)
let to_str_act =
  Set.fold
  ~f:(fun str e -> Caml.Format.sprintf "[%s];%s" (to_str_lst e "←") str)
  ~init:""
  
let to_string (tbl:table) =
  let rec to_string = function
    | (p, a)::t -> Caml.Format.sprintf "%s | {%s} \n%s" 
      (to_str_lst p "=")
      (to_str_act a)
      (to_string t)
    | [] -> ""
  in
  String.drop_suffix (to_string tbl) 2

(*===========================================================================*)
(* Helper functions for formatting HTML code                                 *)
(*===========================================================================*)
let prelude =
  let l =
    [ "table, th, td { border-collapse: collapse; }"
    ; "table, th, td { font-size: 20px; }"
    ; "table.framed { border: 2px solid black; }"
    ; "table.framed th, table.framed td { border: 1px solid black; }"
    ; "th, td { padding: 3px; }"
    ; "tr:nth-child(even) { background-color: #eee; }"
    ; "tr:nth-child(odd) { background-color: #fff; }"
    ; ".align-center { text-align: center; }"
    ; "th { text-align: center; }"
    ]
  in
  Tyxml.Html.style (List.map ~f:Tyxml.Html.txt l)
  
let prelude_str =
  Caml.Format.asprintf "%a@." (Tyxml.Html.pp_elt ()) prelude

let to_string_doc b =
  let meta_str = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">" in
  let footer_str =
    "<script> \
     [...document.querySelectorAll('p')].forEach(el => \
       el.addEventListener('click', () => el.nextSibling.style.display = \
       el.nextSibling.style.display === 'none' ? 'block' : 'none')); \
     [...document.querySelectorAll('ul ul')].forEach(el => \
       el.style.display = 'none'); \
     </script>"
  in
  Caml.Format.asprintf "<head>%s%s</head><body>@[%a@]%s</body>@."
    meta_str
    prelude_str
    (Tyxml.Html.pp_elt ())
    (PrintBox_html.to_html b)
    footer_str
(*===========================================================================*)

(** [render_box tbl] is the [PrintBox.t] object representing [tbl] *)
let render_box tbl =
  let str = to_string tbl in
  let str_lst = String.split str ~on:'\n' in
  let matrix = Array.make_matrix ~dimy:2 ~dimx:(List.length str_lst) "" in
  let _ = List.iteri str_lst ~f:(fun i row -> 
    let h1, h2 = match String.split row ~on:'|' with
                | h1::h2::_ -> h1, h2
                | _ -> failwith "Impossible"
    in
    matrix.(i).(0) <- (String.drop_suffix h1 1);
    matrix.(i).(1) <- (String.drop_prefix h2 1)
  )
  in
  PrintBox.grid_text matrix

let render tbl =
  let box = render_box tbl in
  let output_file =
    Caml.Filename.(temp_file ("Forwarding Table_") (".html"))
    |> String.tr ~target:' ' ~replacement:'-'
  in
  let out = Stdio.Out_channel.create output_file in
  Stdio.Out_channel.output_string out (to_string_doc box);
  Stdio.Out_channel.close out;
  Open.in_default_app output_file |> ignore