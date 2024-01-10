%token LPAR RPAR EOF
%token IMPORT CHECK PRINT EQUIV NEQUIV
%token PLUS AND DOT STAR NEG XOR
%token TST MOD
%token SKIP DROP DUP
%token <string> IDENT
%token <string> FILENAME
%token <int> NUM

%start <Nkcmd.t list> nkpl_file

%%

nkpl_file:
  | r=nkpl_cmd_list; EOF { List.rev r }
  ;

nkpl_cmd_list:
  | r=nkpl_cmd; rs=nkpl_cmd_list { r::rs }
  | { [] }
  ;

nkpl_cmd:
  | IMPORT; fn = FILENAME { Nkcmd.Import fn }
  | CHECK; e1=nkpl_exp; EQUIV; e2=nkpl_exp { Nkcmd.Check (true, e1, e2) }
  | CHECK; e1=nkpl_exp; NEQUIV; e2=nkpl_exp { Nkcmd.Check (false, e1, e2) }
  | PRINT; e=nkpl_exp { Nkcmd.Print e }
  ;

nkpl_exp:
  | r1=irx; PLUS; r2=nkpl_exp { Nkexp.union_pair r1 r2 }
  | r1=irx; XOR; r2=nkpl_exp { Nkexp.xor r1 r2 }
  | r=irx { r }
  ;

irx:
  | r1=drx; AND; r2=irx { Nkexp.intersect_pair r1 r2 }
  | r=drx { r }
  ;

drx:
  | r1=urx; DOT; r2=drx { Nkexp.seq_pair  r1 r2  }
  | c1=cx; r2=drx { Nkexp.seq_pair c1 r2 }
  | r=urx { r }
  ;

urx:
  | r=urx; STAR { Nkexp.star r }
  | r=urx; NEG { Nkexp.neg r }
  | r=arx { r }
  ;

arx:
  | c=cx { c }
  | LPAR; r=nkpl_exp; RPAR { r }
  ;

cx:
  | f = IDENT; TST; v = NUM { Nkexp.Filter (Nkexp.get_or_assign_fid f, Nkexp.value_of_int v) }
  | f = IDENT; MOD; v = NUM { Nkexp.Mod (Nkexp.get_or_assign_fid f, Nkexp.value_of_int v) }
  | DUP { Nkexp.Dup }
  | DROP { Nkexp.Drop }
  | SKIP { Nkexp.Skip }
  ;

%%
