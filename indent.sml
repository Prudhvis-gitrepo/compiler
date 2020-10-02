(*structure Indent = struct

fun prispa 0     = ""
  | prispa space = "  " ^ (prispa (space-1))

fun  indent space (Ast.Var a)		= a
  | indent space (Ast.Const a)		= Int.toString a
  
  | indent space (Ast.Op (a,Ast.Plus,b))	= (indent space a) ^ "+" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Minus,b))	= (indent space a) ^ "-" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Mul,b))	= (indent space a) ^ "*" ^ (indent space b)
  | indent space (Ast.Op (a,Ast.Div,b))	= (indent space a) ^ "/" ^ (indent space b)
  
  | indent space (Ast.If (a,b))		= (prispa space) ^ ("if " ^ (indent space a) ^ " then " ^ (indent (space+2) b))
  
| indent space (Ast.While (a,b))	= (prispa space) ^ "while " ^ (indent space a) ^ " do\n" ^ (indent (space+2) b)
  | indent space (Ast.For (a,b,c,d))    = (prispa space) ^ "for " ^ (indent space a) ^ " := " ^ (indent space b) ^ " to " ^ (indent space c) ^ " do\n" ^ (indent (space+2) d)
  | indent space (Ast.Let (a,b))	= (prispa space) ^ "let\n" ^ (indentlist (space+2) a) ^ (prispa space) ^ "in\n" ^ (indentlist (space+2) b) ^ "\n" ^ (prispa space) ^ "end"
  
  | indent space (Ast.Equal (a,b))	= (indent space a) ^ " = " ^ (indent space b)
  | indent space (Ast.Assign (a,b)) = (prispa space) ^ (indent space a) ^ " := " ^ (indent space b) ^ ";"
    
and indentlist space [] 	= ""
  | indentlist space (x::xs) 	= (indent space x ^ "\n" ^ indentlist space xs)

end
*)
structure Indent = struct

fun prispa 0     = " "
  | prispa spa = " " ^ (prispa (spa-1))

fun Exp spa (Ast.Nil)			= (prispa spa)^"nil\n"
  	| Exp spa (Ast.Const a)			= Int.toString a
 	 | Exp spa (Ast.Quote a)			= a
 	 | Exp spa (Ast.IDENT (Ast.Var a))	= a
 	 | Exp spa (Ast.IF (a,b))		= (prispa spa) ^ "if " ^ (Exp spa a) ^ " then \n" ^ (Exp (spa+2) b)^"\n"
 	 | Exp spa (Ast.ELSE (a,b,c))		= (prispa spa) ^ "if " ^ (Exp spa a) ^ " then\n " ^ (Exp (spa+2) b)^"\n"^(prispa spa) ^"else"^ (Exp (spa+2) c)^"\n"
 	 | Exp spa (Ast.WHILE (a,b))		= (prispa spa) ^ "while " ^ (Exp spa a) ^ " do\n" ^ (Exp (spa+2) b)^"\n"
 	 | Exp spa (Ast.FOR (a,b,c,d))    	= (prispa spa) ^ "for" ^(prispa spa) ^ (Id spa a) ^ ":=" ^ (Exp spa b) ^ " to " ^ (Exp spa c) ^ " do\n" ^ (Exp (spa+2) d)^"\n"
 	 | Exp spa (Ast.Break )			= (prispa spa) ^ "break\n"
 	 | Exp spa (Ast.Let (a,b))		= (prispa spa) ^ "let \n" ^(Decs (spa+2) a)^(prispa spa)^"\n in \n"^(Exps (spa+2) b) ^ "\n" ^(prispa spa) ^ "end\n"
 	 | Exp spa (Ast.UMINUS a)		= "-"^(Exp spa a)
 	 | Exp spa (Ast.OP (a,Ast.PLUS,b))	= (Exp spa a) ^ "+" ^ (Exp spa b)
 	 | Exp spa (Ast.OP (a,Ast.MINUS,b))	= (Exp spa a) ^ "-" ^ (Exp spa b)
 	 | Exp spa (Ast.OP (a,Ast.MUL,b))	= (Exp spa a) ^ "*" ^ (Exp spa b)
 	 | Exp spa (Ast.OP (a,Ast.DIVIDE,b))	= (Exp spa a) ^ "/" ^ (Exp spa b)
 	 | Exp spa (Ast.OP (a,Ast.EQUAL,b))	= (Exp spa a) ^ "=" ^ (Exp spa b)
 	 | Exp spa (Ast.OP (a,Ast.LESSGREAT,b))	= (Exp spa a) ^ "<>" ^ (Exp spa b)
 	 | Exp spa (Ast.OP (a,Ast.GREAT,b))	= (Exp spa a) ^ ">" ^ (Exp spa b)
 	 | Exp spa (Ast.OP (a,Ast.LESS,b))	= (Exp spa a) ^ "<" ^ (Exp spa b)
 	 | Exp spa (Ast.OP (a,Ast.GREATEQUAL,b))= (Exp spa a) ^ ">=" ^ (Exp spa b)
	  | Exp spa (Ast.OP (a,Ast.LESSEQUAL,b))	= (Exp spa a) ^ "<=" ^ (Exp spa b)
	  | Exp spa (Ast.OP (a,Ast.AND,b))	= (Exp spa a) ^ "&" ^ (Exp spa b)
	  | Exp spa (Ast.OP (a,Ast.OR,b))		= (Exp spa a) ^ "|" ^ (Exp spa b)
	  | Exp spa (Ast.Exps a)			= "("^(Exps1 spa a)^")"
	  | Exp spa (Ast.ArrayCreate (a,b))	= (prispa spa)^(Id spa a)^"["^(Exp spa b)^"]"
	  | Exp spa (Ast.RecordCreate (a,b))	= let
             fun prin []      = "" 
	| prin ((a,b)::[])  = (Id spa a)^" = "^( Exp spa b)
	   | prin ((a,b)::x )  = (Id spa a)^" = "^( Exp spa b)^", "^(prin x)		
					                      in
						                          (Id spa a)^"{"^(prin b)^"}\n"	
					                      end
 
  | Exp spa (Ast.New a)			= (prispa spa)^"new "^(Id 0 a)
  | Exp spa (Ast.Functioncall (a,b))	= (prispa spa)^(Id spa a)^"("^(Parguments spa b)^")"
  | Exp spa (Ast.Methodcall (a,b,c))	= (prispa spa)^(Exp spa a)^"."^(Id spa b)^"("^(Parguments spa c)^")"
  | Exp spa (Ast.Assign (a,b)) 	= (prispa spa) ^ (Exp spa a) ^ ":=" ^ (Exp spa b)
  | Exp spa (Ast.Lvalue1 (a,b)) = ( prispa spa) ^ (Exp spa a) ^ "." ^ (Id spa b)
  | Exp spa (Ast.Lvalue2 (a,b)) = ( prispa spa)^ (Exp spa a) ^" [" ^(Exp spa a) ^ "] "

and 
    Id spa (Ast.Var a) = a

and
Parguments spa [] = ""
	|Parguments spa (x::[]) = ( Exp spa x)^""
	|Parguments spa (x::xs) = ( Exp spa x)^", "^(Parguments spa xs)

and 
  Exps spa [] = ""
	 | Exps spa (a :: []) = (Exp spa a)
	 | Exps spa (a :: xs) = (Exp spa a)^";\n "^(Exps spa xs) 

and
  Exps1 spa [] = ""
	 | Exps1 spa (a :: []) = (Exp 0 a)
	 | Exps1 spa (a :: xs) = (Exp 0 a)^"; "^(Exps 0 xs) 

and 
Decs spa [] = ""
	 | Decs spa (a :: []) = (Dec spa a)
	 | Decs spa (a :: xs) = (Dec spa a)^(Decs spa xs)

and 
Dec spa (Ast.Typedec (a,b))	= (prispa spa)^" type "^(Id spa a)^" = "^(TY spa b)
	  | Dec spa (Ast.Classdec1 (a,b))	= (prispa spa)^" class "^(Id spa a)^(prispa spa)^"\n{\n"^(CFS (spa+2) b)^"}"^"\n"
	  | Dec spa (Ast.Classdec2 (a,b,c))	= (prispa spa)^" class "^(Id spa a)^"extends"^(Id spa b)^(prispa spa)^"\n{"^(CFS (spa+2) c)^"}"^"\n"
	  | Dec spa (Ast.Vardec a)	= (prispa spa)^(Var 0 a)
	  | Dec spa (Ast.Functiondec1 (a,b,c))	= (prispa spa)^" function "^(Id spa a)^"("^(TYF spa b)^")"^" = "^(Exp 0 c)^"\n"
	  | Dec spa (Ast.Functiondec2 (a,b,c,d))= (prispa spa)^" function"^(Id spa a)^"("^(TYF spa c)^")"^":"^(Id spa b)^"="^(Exp 0 d)^"\n"
	  | Dec spa (Ast.Primitivedec1 (a,b))	= (prispa spa)^" primitive"^(Id spa a)^"("^(TYF spa b)^")"^"\n"
	  | Dec spa (Ast.Primitivedec2 (a,b,c))	= (prispa spa)^" primitive"^(Id spa a)^"("^(TYF spa c)^")"^":"^(Id spa b)^"\n"
	  | Dec spa (Ast.Import a)		= (prispa spa)^"import "^(Exp 0 a)^"\n"
  
and 
Var spa (Ast.Vardec1 (a,b,c))   =  (prispa 0)^"var "^ (Id spa a)^" : "^(Id spa b)^" := "^(Exp spa c)^"\n"
	  | Var spa (Ast.Vardec2 (a,b))   =  (prispa 0)^"var "^(Id spa a)^" := "^(Exp spa b)^"\n"

and 
CFS spa [] = ""
	 | CFS spa (a :: []) = (CF spa a)
	 | CFS spa (a :: xs) = (CF spa a)^(CFS spa xs)

and 
CF spa (Ast.ClassVar a)	= (prispa spa)^(Var spa a)
	| CF spa (Ast.Method1 (a,b,c))	= (prispa spa)^"method "^(Id spa a)^"("^ (TYF spa b)^")"^" = "^(Exp 0 c)^"\n"
  | CF spa (Ast.Method2 (a,b,c,d))  = (prispa spa)^"method "^(Id spa a)^"("^(TYF spa c)^")"^" : "^(Id spa b)^" = "^ (Exp 0 d)^"\n"


and 
TY spa (Ast.TyIDENT (Ast.Var a))		=  a
	  | TY spa ( Ast.Tyf a)	= " {"^(TYF spa a)^"} "
	  | TY spa (Ast.Array a)	= (prispa spa)^"array of "^(Id spa a)
	  | TY spa (Ast.Classfields a)	= (prispa spa)^"\n class \n{\n"^(CFS (spa+2) a) ^"} \n"
  	  | TY spa (Ast.Classfields2 (a,b))	= (prispa spa)^"class extends "^(Id spa a)^"\n{\n"^(CFS (spa+2) b) ^"} \n"

and 
TYF spa (Ast.Tyfields a)	= let
	fun p [] = " "|p ((a,b)::[]) = (Id spa a)^" : "^(Id spa a)|p ((a,b)::xs) = (Id spa a)^" : "^(Id spa a)^" , "^(p xs)
	in	(p a)
	end
and 
Program spa (Ast.Expression a)	= (Exp spa a)
	  | Program spa (Ast.Decls a) 	= (Decs spa a)

end
