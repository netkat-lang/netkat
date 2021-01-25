%token LPAR RPAR EOF
%token PLUS DOT STAR (* QMARK *)
%token E
%token <int> NUM

%start <Intrx.t> rx_eof

%%

rx_eof:
  | r=rx; EOF { r }
  ;

rx:
  | r=prx { r }

prx:
  | r1=drx; PLUS; r2=prx { Intrx.union_pair r1 r2 }
  | r=drx { r }

drx:
  | r1=urx; DOT; r2=drx { Intrx.seq_pair r1 r2 }
  | c1=cx; r2=drx { Intrx.seq_pair c1 r2 }
  | r=urx { r }

urx:
  (* | r=urx; QMARK { Intrx.QMark r } *)
  | r=urx; STAR { Intrx.star r }
  | r=arx { r }

arx:
  | c=cx { c }
  | LPAR; r=rx; RPAR { r }
  ;

cx:
  | n = NUM { Intrx.Char (n) }
  | E { Intrx.Epsilon }
  ;

%%
