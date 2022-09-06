
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | X
    | STAR
    | RPAR
    | QMARK
    | PLUS
    | NUM of (
# 4 "lib/rx_parser.mly"
       (int)
# 20 "lib/rx_parser.ml"
  )
    | NEG
    | LPAR
    | EOF
    | E
    | DOT
    | AND
  
end

include MenhirBasics

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_rx_eof) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: rx_eof. *)

  | MenhirState03 : (('s, _menhir_box_rx_eof) _menhir_cell1_LPAR, _menhir_box_rx_eof) _menhir_state
    (** State 03.
        Stack shape : LPAR.
        Start symbol: rx_eof. *)

  | MenhirState09 : (('s, _menhir_box_rx_eof) _menhir_cell1_urx, _menhir_box_rx_eof) _menhir_state
    (** State 09.
        Stack shape : urx.
        Start symbol: rx_eof. *)

  | MenhirState11 : (('s, _menhir_box_rx_eof) _menhir_cell1_cx, _menhir_box_rx_eof) _menhir_state
    (** State 11.
        Stack shape : cx.
        Start symbol: rx_eof. *)

  | MenhirState18 : (('s, _menhir_box_rx_eof) _menhir_cell1_irx, _menhir_box_rx_eof) _menhir_state
    (** State 18.
        Stack shape : irx.
        Start symbol: rx_eof. *)

  | MenhirState21 : (('s, _menhir_box_rx_eof) _menhir_cell1_drx, _menhir_box_rx_eof) _menhir_state
    (** State 21.
        Stack shape : drx.
        Start symbol: rx_eof. *)


and ('s, 'r) _menhir_cell1_cx = 
  | MenhirCell1_cx of 's * ('s, 'r) _menhir_state * (Intrx.t)

and ('s, 'r) _menhir_cell1_drx = 
  | MenhirCell1_drx of 's * ('s, 'r) _menhir_state * (Intrx.t)

and ('s, 'r) _menhir_cell1_irx = 
  | MenhirCell1_irx of 's * ('s, 'r) _menhir_state * (Intrx.t)

and ('s, 'r) _menhir_cell1_urx = 
  | MenhirCell1_urx of 's * ('s, 'r) _menhir_state * (Intrx.t)

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state

and _menhir_box_rx_eof = 
  | MenhirBox_rx_eof of (Intrx.t) [@@unboxed]

let _menhir_action_01 =
  fun c ->
    (
# 37 "lib/rx_parser.mly"
         ( c )
# 88 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_02 =
  fun r ->
    (
# 38 "lib/rx_parser.mly"
                     ( r )
# 96 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_03 =
  fun n ->
    (
# 42 "lib/rx_parser.mly"
            ( Intrx.Char (n) )
# 104 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_04 =
  fun () ->
    (
# 43 "lib/rx_parser.mly"
      ( Intrx.(union_pair (Char 0) (Char 1)) )
# 112 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_05 =
  fun () ->
    (
# 44 "lib/rx_parser.mly"
      ( Intrx.Epsilon )
# 120 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_06 =
  fun r1 r2 ->
    (
# 26 "lib/rx_parser.mly"
                        ( Intrx.seq_pair r1 r2 )
# 128 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_07 =
  fun c1 r2 ->
    (
# 27 "lib/rx_parser.mly"
                  ( Intrx.seq_pair c1 r2 )
# 136 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_08 =
  fun r ->
    (
# 28 "lib/rx_parser.mly"
          ( r )
# 144 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_09 =
  fun r1 r2 ->
    (
# 22 "lib/rx_parser.mly"
                        ( Intrx.intersect_pair r1 r2 )
# 152 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_10 =
  fun r ->
    (
# 23 "lib/rx_parser.mly"
          ( r )
# 160 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_11 =
  fun r1 r2 ->
    (
# 18 "lib/rx_parser.mly"
                         ( Intrx.union_pair r1 r2 )
# 168 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_12 =
  fun r ->
    (
# 19 "lib/rx_parser.mly"
          ( r )
# 176 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_13 =
  fun r ->
    (
# 15 "lib/rx_parser.mly"
          ( r )
# 184 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_14 =
  fun r ->
    (
# 11 "lib/rx_parser.mly"
              ( r )
# 192 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_15 =
  fun r ->
    (
# 31 "lib/rx_parser.mly"
                 ( Intrx.qmark r )
# 200 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_16 =
  fun r ->
    (
# 32 "lib/rx_parser.mly"
                ( Intrx.star r )
# 208 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_17 =
  fun r ->
    (
# 33 "lib/rx_parser.mly"
               ( Intrx.neg r )
# 216 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_action_18 =
  fun r ->
    (
# 34 "lib/rx_parser.mly"
          ( r )
# 224 "lib/rx_parser.ml"
     : (Intrx.t))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AND ->
        "AND"
    | DOT ->
        "DOT"
    | E ->
        "E"
    | EOF ->
        "EOF"
    | LPAR ->
        "LPAR"
    | NEG ->
        "NEG"
    | NUM _ ->
        "NUM"
    | PLUS ->
        "PLUS"
    | QMARK ->
        "QMARK"
    | RPAR ->
        "RPAR"
    | STAR ->
        "STAR"
    | X ->
        "X"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_16_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _v _tok ->
      let _v =
        let r = _v in
        _menhir_action_13 r
      in
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let r = _v in
          let _v = _menhir_action_14 r in
          MenhirBox_rx_eof _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | X ->
          let _menhir_stack = MenhirCell1_cx (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_04 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
      | NUM _v_1 ->
          let _menhir_stack = MenhirCell1_cx (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v_1 in
          let _v = _menhir_action_03 n in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
      | LPAR ->
          let _menhir_stack = MenhirCell1_cx (_menhir_stack, _menhir_s, _v) in
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | E ->
          let _menhir_stack = MenhirCell1_cx (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
      | AND | DOT | EOF | NEG | PLUS | QMARK | RPAR | STAR ->
          let c = _v in
          let _v = _menhir_action_01 c in
          _menhir_goto_arx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | X ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_04 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | NUM _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_03 n in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | E ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_arx : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let r = _v in
      let _v = _menhir_action_18 r in
      _menhir_goto_urx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_urx : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let r = _v in
          let _v = _menhir_action_16 r in
          _menhir_goto_urx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | QMARK ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let r = _v in
          let _v = _menhir_action_15 r in
          _menhir_goto_urx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | NEG ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let r = _v in
          let _v = _menhir_action_17 r in
          _menhir_goto_urx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | DOT ->
          let _menhir_stack = MenhirCell1_urx (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | X ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_04 () in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
          | NUM _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_1 in
              let _v = _menhir_action_03 n in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | E ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_05 () in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
          | _ ->
              _eRR ())
      | AND | EOF | PLUS | RPAR ->
          let r = _v in
          let _v = _menhir_action_08 r in
          _menhir_goto_drx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_drx : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState21 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState18 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_20 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND ->
          let _menhir_stack = MenhirCell1_drx (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | X ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_04 () in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
          | NUM _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_1 in
              let _v = _menhir_action_03 n in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
          | E ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_05 () in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState21 _tok
          | _ ->
              _eRR ())
      | EOF | PLUS | RPAR ->
          let r = _v in
          let _v = _menhir_action_10 r in
          _menhir_goto_irx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_irx : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState21 ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState18 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_22 : type  ttv_stack. (ttv_stack, _menhir_box_rx_eof) _menhir_cell1_drx -> _ -> _ -> _ -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_drx (_menhir_stack, _menhir_s, r1) = _menhir_stack in
      let r2 = _v in
      let _v = _menhir_action_09 r1 r2 in
      _menhir_goto_irx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_17 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_irx (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | X ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_04 () in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
          | NUM _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let n = _v_1 in
              let _v = _menhir_action_03 n in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
          | E ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_05 () in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
          | _ ->
              _eRR ())
      | EOF | RPAR ->
          let r = _v in
          let _v = _menhir_action_12 r in
          _menhir_goto_prx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_prx : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_rx_eof) _menhir_state -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState18 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_16_spec_00 _menhir_stack _v _tok
      | MenhirState03 ->
          _menhir_run_16_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_19 : type  ttv_stack. (ttv_stack, _menhir_box_rx_eof) _menhir_cell1_irx -> _ -> _ -> _ -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_irx (_menhir_stack, _menhir_s, r1) = _menhir_stack in
      let r2 = _v in
      let _v = _menhir_action_11 r1 r2 in
      _menhir_goto_prx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_16_spec_03 : type  ttv_stack. (ttv_stack, _menhir_box_rx_eof) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let r = _v in
        _menhir_action_13 r
      in
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let r = _v in
          let _v = _menhir_action_02 r in
          _menhir_goto_arx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_rx_eof) _menhir_cell1_cx -> _ -> _ -> _ -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_cx (_menhir_stack, _menhir_s, c1) = _menhir_stack in
      let r2 = _v in
      let _v = _menhir_action_07 c1 r2 in
      _menhir_goto_drx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_rx_eof) _menhir_cell1_urx -> _ -> _ -> _ -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_urx (_menhir_stack, _menhir_s, r1) = _menhir_stack in
      let r2 = _v in
      let _v = _menhir_action_06 r1 r2 in
      _menhir_goto_drx _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_rx_eof =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | X ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_04 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | NUM _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_03 n in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | E ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | _ ->
          _eRR ()
  
end

let rx_eof =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_rx_eof v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 47 "lib/rx_parser.mly"
  

# 561 "lib/rx_parser.ml"
