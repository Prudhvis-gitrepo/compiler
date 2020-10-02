(*
structure tigh = struct
val inti = tiglex.makeLexer (fn _ => TextIO.inputN (TextIO.stdIn,1))


fun lexfile file = let val stream = TextIO.openIn file
           in tiglex.makeLexer (fn n => TextIO.inputN(stream,n))
           end

fun tokentostr token = case token of
          (Const x) => Int.toString(x)
        | (String x) => x
        | (K Array) => "array"
        | (K If) => "if"
        | (K Then) => "then"
        | (K Else) => "else"
        | (K While) => "while"
        | (K For) => "for"
        | (K Let) => "let"
        | (K To) => "to"
        | (K Do) => "do"
        | (K In) => "in"
        | (K End) => "end"
        | (K Of) => "of"
        | (K Break) => "break"
        | (K Nil) => "nil"
        | (K Function) => "function"
        | (K Var) => "var"
        | (K Type) => "type"
        | (K Import) => "import"
        | (K Primitive) => "primitive"
        | (O Class) => "class"
        | (O Extends) => "extends"
        | (O Method) => "method"
        | (O New) => "new"
        | (S Comma) => ","
        | (S Colon) => ":"
        | (S Semicolon) => ";"
        | (S OpenParanthese) => "("
        | (S CloseParanthese) => ")"
        | (S OpenBracket) => "["
        | (S CloseBracket) => "]"
        | (S OpenBrace) => "{"
        | (S CloseBrace) => "}"
        | (S Dot) => "."
        | (S Plus) => "+"
        | (S Minus) => "-"
        | (S Mul) => "*"
        | (S Div) => "/"
        | (S Equal) => "="
        | (S AngledBrackets) => "<>"
        | (S LessThan) => "<"
        | (S LessThanEqual) => "<="
        | (S GreaterThan) => ">"
        | (S GreaterThanEqual) => ">="
        | (S AND) => "&"
        | (S OR) => "|"
        | (S ColonEqual) => ":="
	| (W Tab) => "\t"
	| (W Space) => " "
        | (Newline) => "\n"
        | (END) => ""
       
fun styleprint token = case token of
         (Const x) => print("\027[87m"^(tokentostr token))
        |(String x) => print("\027[90m"^(tokentostr token))
        |(K _) => print("\027[34m"^(tokentostr token))
        |(O _) => print("\027[37m"^(tokentostr token))
        |(S _) => print("\027[35m"^(tokentostr token))
	|(W _) => print(tokentostr token)
	|(Newline) => print(tokentostr token)
        |(END) => print("\027[0m"^tokentostr token)

fun parallel lexer = let fun looping stack = case lexer () of
                          END      => styleprint(END)
                           |  token    => looping (styleprint token)
             in looping ()
             end
val _ =  ( case CommandLine.arguments() of
           [] => parallel inti
        |  xs => (List.map (parallel o lexfile) xs; ())
     )
end

*)

structure Tigh = struct 

val interactive = tiglex.makeLexer (fn _ => TextIO.inputN (TextIO.stdIn,1))

fun lexfile file = let val strm = TextIO.openIn file
		   in tiglex.makeLexer (fn n => TextIO.inputN(strm,n))
		   end

fun whitespace(1,Space) = " "
	| whitespace(n,Space) = " "^(whitespace(n-1,Space));

fun tokentostr token = case token of 
			(Comments x) => x
		| (Const x) => Int.toString (x)
		| (Quote x) => x
		| (Newline) => "\n"
		| (END) => ""
		| (Char (x,Space)) => whitespace(x,Space)
		| (Text x)  => x
		| (Key Array) => "array"
		| (Key If) => "if"
		| (Key Else) => "else"
		| (Key While) => "while"
		| (Key For) => "for"
		| (Key To) => "to"
		| (Key Do) => "do"
		| (Key Let) => "let"
		| (Key In) => "in"
		| (Key End) => "end"
		| (Key Of) => "of"
		| (Key Break) => "break"
		| (Key Nil) => "nil"
		| (Key Function) => "function"
		| (Key Var) => "var"
		| (Key Type) => "type"
		| (Key Import) => "import"
		| (Key Primitive) => "primitive"
		| (Obj Class) => "class"
		| (Obj Extends) => "extends"
		| (Obj Method) => "method"
		| (Obj New) => "new"
		| (Sym Coma) => ","
		| (Sym Colon) => ":"
		| (Sym Semicolon) => ";"
		| (Sym Lbrac) => "("
		| (Sym Rbrac) => "("
		| (Sym Lsqbrac) => "["
		| (Sym Rsqbrac) => "]"
		| (Sym Lflbrac) => "{"
		| (Sym Rflbrac) => "}"
		| (Sym Dot) => "."
		| (Sym Plus) => "+"
		| (Sym Minus) => "-"
		| (Sym Mul) => "*"
		| (Sym Div) => "/"
		| (Sym Equalto) => "="
		| (Sym LtGt) => "<>"
		| (Sym Lt) => "<"
		| (Sym Lteq) => "<="
		| (Sym Gt) => ">"
		| (Sym Gteq) => ">="
		| (Sym AND) => "&"
		| (Sym OR) => "|"
		| (Sym ColonEq) => ":="
		



fun styleprint token = case token of 
			(Comments x)  => print("\027[87m"^(tokentostr token))
			| (Quote x)   => print("\027[90m"^(tokentostr token))
			| (Newline)   => print(tokentostr token)
			| (Char(x,Space)) => print(tokentostr token)
			| (Const x)   => print("\027[34m"^(tokentostr token))
			| (Text x)    => print("\027[37m"^(tokentostr token))
			| (Key _)    => print("\027[94m"^(tokentostr token))
			| (Obj _)    => print("\027[35m"^(tokentostr token))
			| (Sym _)    => print("\027[37m"^(tokentostr token))
			| (END)      => print("\027[0m"^(tokentostr token))
			
 



fun paralleltoLexer lexer = let fun looping stack = case lexer () of
						END     => styleprint (END)
						| token => looping(styleprint token)
			
			 in looping ()
			 end


val _ =  ( case CommandLine.arguments() of
	       [] => parallleltolexer interactive
	    |  xs => (List.map (paralleltoLexer o lexfile) xs; ())
	 )

end

