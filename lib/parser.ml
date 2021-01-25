include Nice_parser.Make(struct
  type result = Intrx.t
  type token = Rx_parser.token
  exception ParseError = Rx_parser.Error
  let parse = Rx_parser.rx_eof
  include Rx_lexer
end)
