type test = { name: string; value: bool }
type act = test
type exp = (act, test) Ast.exp
type bexp = test Ast.bexp