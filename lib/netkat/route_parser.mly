%{

%}

%token <Route.prefix> PREFIX
%token <Route.ip> QUAD
%token EOF

(*
%token DROP
%token RCV
%token ATTCH
%token KEYW
*)

%start <Route.t list> route_file

%%

route_file:
  | rs=routes; EOF { rs }
  ;

routes:
  | r=route; rs=routes { r::rs }
  | { [] }
  ;

route:
  | p=PREFIX; hop=QUAD { (p, hop) }
  | p=QUAD; hop=QUAD { ((p,32), hop) } (* Fully specified IPs are /32 prefixes *)
  ;

%%
