%{
let constants = ((Hashtbl.create 10) : (string,int) Hashtbl.t)
let macros = ((Hashtbl.create 10) : (string,Nkexp.t) Hashtbl.t)

let get_constant s = Hashtbl.find_opt constants s

let get_macro s = match (Hashtbl.find_opt macros s) with
| None -> Nkexp.Var(s)
| Some(e) -> e

let add_constant s i = Hashtbl.replace constants s i
let add_macro s e = Hashtbl.replace macros s e
%}

%token LPAR RPAR EOF
%token RANGESUM IMPORT CHECK PRINT TIKZ EQUIV NEQUIV FOR DO IN DOTDOT
%token ARROW LAMBDA
%token PLUS DIFF AND DOT STAR NEG XOR
%token FWD BWD EXISTS FORALL REP LBRACE RBRACE COM
%token NTST TST MOD
%token SKIP DROP DUP
%token NEWLINE
%token <string> IDENT
%token <string> FILENAME
%token <int> NUM

%start <Nkcmd.t list> nkpl_file
%start <Nkcmd.t option> single_cmd
%start <Nkexp.t> single_exp

%%

nkpl_file:
  | NEWLINE*; r=nkpl_cmd_list; EOF { r }
  ;

single_cmd:
  | r=nkpl_cmd; EOF { Some r }
  | EOF { None }
  ;

single_exp:
  | r=nk_sum; EOF { r }
  ;

nkpl_cmd_list:
  | r=nkpl_cmd; NEWLINE* { [r] }
  | r=nkpl_cmd; NEWLINE+; rs=nkpl_cmd_list { r::rs }
  ;

nkpl_cmd:
  | IMPORT; fn = FILENAME { Nkcmd.Import fn }
  | CHECK; e1=nk_exp; EQUIV; e2=nk_exp { Nkcmd.Check (true, e1, e2) }
  | CHECK; e1=nk_exp; NEQUIV; e2=nk_exp { Nkcmd.Check (false, e1, e2) }
  | PRINT; e=nk_exp { Nkcmd.Print e }
  | TIKZ; e=nk_exp { Nkcmd.Tikz e }
  | var=IDENT; TST; e=nk_exp { add_macro var e; Nkcmd.Let (var,e) }
  | var=IDENT; TST; v=NUM { add_constant var v; Nkcmd.VLet (var, Value.of_int v) }
  | REP; e=nk_exp { Nkcmd.Rep e }
  | FOR; var=IDENT; IN; i_0=NUM; DOTDOT; i_n=NUM; DO; c=nkpl_cmd { Nkcmd.For (var, i_0, i_n, c) }
  (*| e=nk_exp { print_string ">> EXPR\n"; Nkcmd.Print e } *)
  ;

field:
  | f=IDENT; TST; v=NUM { (Field.get_or_assign_fid f, Value.of_int v) }

fields:
  | { Pk.empty }
  | f=field; fs=fields { let (a,b) = f in Pk.add fs a b }
  | f=field; COM; fs=fields { let (a,b) = f in Pk.add fs a b }

packet:
  | LBRACE; fs=fields; RBRACE { fs }

nk_exp:
  | FWD; e=nk_exp { Nkexp.fwd e }
  | FWD; p=packet; e=nk_exp { Nkexp.Fwd(Some(p),e) }
  | BWD; e=nk_exp  { Nkexp.bwd e }
  | BWD; p=packet; e=nk_exp { Nkexp.Bwd(Some(p),e) }
  | LAMBDA; v=IDENT; ARROW; e=nk_exp { Nkexp.lambda v e }
  | FORALL; f=IDENT; e=nk_exp { Nkexp.forall (Field.get_or_assign_fid f) e }
  | EXISTS; f=IDENT; e=nk_exp { Nkexp.exists (Field.get_or_assign_fid f) e }
  | e=nk_sum { e }
  ;

nk_sum:
  | r1=nk_diff; PLUS; r2=nk_sum { Nkexp.union_pair r1 r2 }
  | r=nk_diff { r }
  ;

nk_diff:
  | r1=nk_conj; DIFF; r2=nk_diff { Nkexp.diff r1 r2 }
  | r1=nk_conj; XOR; r2=nk_diff { Nkexp.xor r1 r2 }
  | r=nk_conj { r }
  ;

nk_conj:
  | r1=nk_seq; AND; r2=nk_conj { Nkexp.intersect_pair r1 r2 }
  | r=nk_seq { r }
  ;

nk_seq:
  | r1=nk_un; DOT; r2=nk_seq { Nkexp.seq_pair  r1 r2  }
  | r=nk_un { r }
  ;

nk_un:
  | r=nk_un; STAR { Nkexp.star r }
  | r=nk_par; m=list(nk_val) {
    match m with
    | [] -> r
    | a::more -> List.fold_left (fun acc a2 -> Nkexp.app acc a2) (Nkexp.app r a) more
  }
  ;

nk_val:
  | v=NUM { Nkexp.Num(v) }
  | c=nk_at { c }
  | LPAR; r=nk_exp; RPAR { r }
  ;

nk_par:
  | c=nk_at { c }
  | NEG; p=nk_par { Nkexp.neg p }
  | LPAR; r=nk_exp; RPAR { r }
  ;

nk_at:
  | RANGESUM; f = IDENT; v1 = NUM; DOTDOT; v2 = NUM { Nkexp.drop (* TODO XXX *) }
  | f = IDENT; TST; v = NUM { Nkexp.filter true (Field.get_or_assign_fid f) (Value.of_int v) }
  | f = IDENT; NTST; v = NUM { Nkexp.filter false (Field.get_or_assign_fid f) (Value.of_int v) }
  | f = IDENT; MOD; v = NUM { Nkexp.modif (Field.get_or_assign_fid f) (Value.of_int v) }
  | f = IDENT; TST; v = IDENT { (*match get_constant v with Some(i) -> Nkexp.filter true (Field.get_or_assign_fid f) (Value.of_int i) | None ->*) Nkexp.vfilter true (Field.get_or_assign_fid f) v }
  | f = IDENT; NTST; v = IDENT { (*match get_constant v with Some(i) -> Nkexp.filter false (Field.get_or_assign_fid f) (Value.of_int i) | None ->*) Nkexp.vfilter false (Field.get_or_assign_fid f) v }
  | f = IDENT; MOD; v = IDENT { (*match get_constant v with Some(i) -> Nkexp.modif (Field.get_or_assign_fid f) (Value.of_int i) | None ->*) Nkexp.vmodif (Field.get_or_assign_fid f) v }
  | v=IDENT { Nkexp.Var(v)(*(get_macro v)*) }
  | DUP { Nkexp.dup }
  | DROP { Nkexp.drop }
  | SKIP { Nkexp.skip }
  ;

%%
