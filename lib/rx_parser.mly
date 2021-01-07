%token LPAR RPAR EOF
%token PLUS DOT STAR QMARK
%token NUM E

%start <Rx.t> rx_eof

%%

rx_eof:
  | r=rx; EOF { Rx.optimize r }
  ;

rx:
  | r=prx { r }

prx:
  | r1=drx; PLUS; r2=prx { Rx.Union(r1,r2) }
  | r=drx { r }

drx:
  | r1=urx; DOT; r2=drx { Rx.Seq(r1,r2) }
  | c1=cx; r2=drx { Rx.Seq(c1,r2) }
  | r=urx { r }

urx:
  | r=urx; QMARK { Rx.QMark r }
  | r=urx; STAR { Rx.Star r }
  | r=arx { r }

arx:
  | c=cx { c }
  | LPAR; r=rx; RPAR { r }
  ;

cx:
  | n = NUM { Rx.Char (n) }
  | E { Rx.Epsilon }
  ;

%%
