%{
open Pk
%}

%token LPAR RPAR EOF
%token IMPORT CHECK PRINT EQUIV NEQUIV
%token PLUS AND DOT STAR NEG XOR
%token FWD BWD
%token NTST TST MOD
%token SKIP DROP DUP
%token <string> VAR
%token <string> IDENT
%token <string> FILENAME
%token <int> NUM

%start <Nkcmd.t list> nkpl_file

%%

nkpl_file:
  | r=nkpl_cmd_list; EOF { r }
  ;

nkpl_cmd_list:
  | r=nkpl_cmd; rs=nkpl_cmd_list { r::rs }
  | { [] }
  ;

nkpl_cmd:
  | IMPORT; fn = FILENAME { Nkcmd.Import fn }
  | CHECK; e1=nk_exp; EQUIV; e2=nk_exp { Nkcmd.Check (true, e1, e2) }
  | CHECK; e1=nk_exp; NEQUIV; e2=nk_exp { Nkcmd.Check (false, e1, e2) }
  | PRINT; e=nk_exp { Nkcmd.Print e }
  | var=VAR; TST; e=nk_exp { Env.bind var e; Nkcmd.Let (var,e) }
  ;

nk_exp:
  | FWD; e=nk_exp { Nkexp.Fwd e }
  | BWD; e=nk_exp { Nkexp.Bwd e }
  | e=nk_sum { e }

nk_sum:
  | r1=nk_conj; PLUS; r2=nk_sum { Nkexp.union_pair r1 r2 }
  | r1=nk_conj; XOR; r2=nk_sum { Nkexp.xor r1 r2 }
  | r=nk_conj { r }
  ;

nk_conj:
  | r1=nk_seq; AND; r2=nk_conj { Nkexp.intersect_pair r1 r2 }
  | r=nk_seq { r }
  ;

nk_seq:
  | r1=nk_un; DOT; r2=nk_seq { Nkexp.seq_pair  r1 r2  }
  | c1=nk_at; r2=nk_seq { Nkexp.seq_pair c1 r2 }
  | r=nk_un { r }
  ;

nk_un:
  | r=nk_un; STAR { Nkexp.star r }
  | r=nk_un; NEG { Nkexp.neg r }
  | r=nk_par { r }
  ;

nk_par:
  | c=nk_at { c }
  | LPAR; r=nk_exp; RPAR { r }
  ;

nk_at:
  | f = IDENT; TST; v = NUM { Nkexp.filter true (get_or_assign_fid f) (value_of_int v) }
  | f = IDENT; NTST; v = NUM { Nkexp.filter false (get_or_assign_fid f) (value_of_int v) }
  | f = IDENT; MOD; v = NUM { Nkexp.modif (get_or_assign_fid f) (value_of_int v) }
  | var=VAR { Env.lookup var }
  | DUP { Nkexp.dup }
  | DROP { Nkexp.drop }
  | SKIP { Nkexp.skip }
  ;

%%
