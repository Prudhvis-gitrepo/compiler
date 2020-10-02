datatype keywords = Array | If | Then | Else | While | For | To | Do | Let | In | End | Of | Break | Nil | Function | Var | Type | Import | Primitive

datatype objRelKeywords= Class | Extends | Method | New

datatype symbols = Comma | Colon | Semicolon | OpenParanthese | CloseParanthese | OpenBracket | CloseBracket | OpenBrace | CloseBrace | Dot | Plus | Minus | Mul | Div | Equal | AngledBrackets | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | AND | OR | ColonEqual

datatype whiteCharacters = Space | Tab

datatype Token = K of keywords | O of objRelKeywords | S of symbols | W of whiteCharacters | Newline | END | Const of int | String of string

