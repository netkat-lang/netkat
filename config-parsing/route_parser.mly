%{
%}

%token <Route.prefix> PREFIX
%token <Route.ip> QUAD
%token RCV ATTCH DROP
%token (*EOL*) EOF

(*
%token DROP
%token RCV
%token ATTCH
%token KEYW
*)

%start <Route.t list> route_file

(* %start <Route.action list> actions *)
(* %start <Route.action list> more_actions *)

%%

route_file:
  | (*skip column headers*) rs=routes; EOF { rs }
  ;

routes:
  | p=PREFIX; acts=actions; rs=routes { (p,acts)::rs }
  | { [] }
  ;

actions:
  | hop=QUAD; acts=actions { Route.Hop(hop)::acts }
  | DROP; acts=actions { Route.Drop::acts }
  | RCV; acts=actions { Route.Rcv::acts }
  | ATTCH; acts=actions { Route.Attch::acts }
  | { [] }

(*
more_actions:
  | EOL; acts=actions { acts }
  | { [] }
  ;
  *)

%%
