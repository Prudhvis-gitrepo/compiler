structure Ast = struct

datatype Program = Expression of EXP
		| Decls of 	DEC list

and EXP = Nil
	| Const of int
	| Quote of string
	| IDENT of Id
	| IF of EXP * EXP
	| ELSE of EXP * EXP * EXP
	| WHILE of EXP * EXP
	| FOR of Id * EXP * EXP * EXP
	| Break
	| Let of DEC list * EXP list
	| OP of EXP * BinOp * EXP
	| UMINUS of EXP
	| Exps of EXP list
	| ArrayCreate of Id * EXP
	| RecordCreate of Id * (Id * EXP)list
	| New of Id
	| Functioncall of ( Id * EXP list)
	| Lvalue1 of (EXP * Id)
	| Lvalue2 of (EXP * EXP)
	| Assign of (EXP * EXP)
	| Methodcall of (EXP * Id * EXP list)

and Id = Var of string

and BinOp = PLUS 
	| MINUS 
	| MUL 
	| DIVIDE 
	| EQUAL 
	| LESSGREAT 
	| LESS 
	| GREAT
	| GREATEQUAL
	| LESSEQUAL
	| AND 
	| OR

and DEC = Import of EXP
	| Typedec of Id * Type
	| Functiondec1 of Id * TypeField * EXP
	| Functiondec2 of Id * Id * TypeField * EXP
	| Primitivedec1 of Id * TypeField
	| Primitivedec2 of Id * Id * TypeField
	| Classdec1 of Id * ClassField list
	| Classdec2 of Id * Id * ClassField list
	| Vardec	of VarDec

and TypeField = Tyfields of (Id * Id ) list

and Type = TyIDENT of Id
	| Tyf of TypeField
	| Array of Id
	| Classfields of ClassField list
	| Classfields2 of Id * ClassField list

and VarDec = Vardec1 of ( Id * Id * EXP)
	| Vardec2 of ( Id * EXP)

and ClassField = ClassVar of VarDec
	| Method1 of Id * TypeField * EXP
	| Method2 of Id *Id * TypeField * EXP



fun plus a b			= OP (a,PLUS,b)
fun minus a b			= OP (a,MINUS,b)
fun mul a b			= OP (a,MUL,b)
fun divide a b			= OP (a,DIVIDE,b)
fun equal a b			= OP (a,EQUAL,b)
fun lessgreat a b		= OP (a,LESSGREAT,b)
fun less a b			= OP (a,LESS,b)
fun great a b			= OP (a,GREAT,b)
fun lessequal a b		= OP (a,LESSEQUAL,b)
fun greatequal a b		= OP (a,GREATEQUAL,b)
fun And a b			= OP (a,AND,b)
fun or a b			= OP (a,OR,b)
fun If a b 			= IF (a, b)
fun Else a b c 			= ELSE (a, b, c)
fun While a b 			= WHILE (a, b)
fun For a b c d			= FOR (a, b, c, d)
end

