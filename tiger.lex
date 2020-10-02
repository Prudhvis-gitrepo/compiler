
type lineNo            = int
type pos               = lineNo
val  lineRef : pos ref = ref 0

fun updateLine n      = lineRef := !(lineRef) + n

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token


fun lineRange l r = "line " ^ l
				  
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")

fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)



fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode

%%

%header (functor ExprLexFun(structure Tokens : Expr_TOKENS));
digit = [0-9];
str = [a-zA-Z];
ws = [\ \t];
ex  = [a-zA-Z]([a-z_A-Z1-9])*;

%%

\n({ws}*\n)*		=> (lex());
{ws}+			=> (lex());
{digit}+		=> (Tokens.CONST(toInt yytext, !lineRef, !lineRef));

"\"".*"\""		=> (Tokens.QUOTE(yytext,!lineRef,!lineRef));
":="			=> ( Tokens.ASSIGN( !lineRef, !lineRef));
"+"			=> ( Tokens.PLUS( !lineRef, !lineRef));
"-"			=> ( Tokens.MINUS( !lineRef, !lineRef));
"*"			=> ( Tokens.MUL( !lineRef , !lineRef));
"/"			=> ( Tokens.DIV( !lineRef, !lineRef));
";"			=> ( Tokens.SEMICOLON( !lineRef, !lineRef));
"="			=> ( Tokens.EQUAL( !lineRef, !lineRef));
","			=> (Tokens.COMMA(!lineRef,!lineRef));
":"			=> (Tokens.COLON(!lineRef,!lineRef));
"("			=> (Tokens.LP(!lineRef,!lineRef));
")"			=> (Tokens.RP(!lineRef,!lineRef));
"["		        => (Tokens.LB(!lineRef,!lineRef));
"]"		        => (Tokens.RB(!lineRef,!lineRef));
"{"		        => (Tokens.LCB(!lineRef,!lineRef));
"}"		        => (Tokens.RCB(!lineRef,!lineRef));
"."		        => (Tokens.DOT(!lineRef,!lineRef));
"<>"		        => (Tokens.LESSGREAT(!lineRef,!lineRef));
"<"		        => (Tokens.LESS(!lineRef,!lineRef));
"<="		        => (Tokens.LESSEQUAL(!lineRef,!lineRef));
">"		        => (Tokens.GREAT(!lineRef,!lineRef));
">="		        => (Tokens.GREATEQUAL(!lineRef,!lineRef));
"&"		      	=> (Tokens.AND(!lineRef,!lineRef));
"|"		        => (Tokens.OR(!lineRef,!lineRef));


"if"			=> ( Tokens.IF( !lineRef, !lineRef));
"else"			=> ( Tokens.ELSE( !lineRef, !lineRef));
"then"	  		=> ( Tokens.THEN( !lineRef, !lineRef));
"while"			=> ( Tokens.WHILE( !lineRef, !lineRef));
"for"			=> ( Tokens.FOR( !lineRef, !lineRef));
"to"			=> ( Tokens.TO( !lineRef, !lineRef));
"do"			=> ( Tokens.DO( !lineRef, !lineRef));
"let"			=> ( Tokens.LET( !lineRef, !lineRef));
"in"			=> ( Tokens.IN( !lineRef, !lineRef));
"end"			=> ( Tokens.END( !lineRef, !lineRef));
"array"			=> (Tokens.ARRAY(!lineRef, !lineRef));
"of"			=> (Tokens.OF(!lineRef,!lineRef));
"break"			=> (Tokens.BREAK(!lineRef,!lineRef));
"nil"			=> (Tokens.NIL(!lineRef,!lineRef));
"function"		=> (Tokens.FUNCTION(!lineRef,!lineRef));
"var"			=> (Tokens.VAR(!lineRef,!lineRef));
"type"			=> (Tokens.TYPE(!lineRef,!lineRef));
"import"		=> (Tokens.IMPORT(!lineRef,!lineRef));
"primitive"		=> (Tokens.PRIMITIVE(!lineRef,!lineRef));

"class"			=> (Tokens.CLASS(!lineRef,!lineRef));
"extends"		=> (Tokens.EXTENDS(!lineRef,!lineRef));
"method"		=> (Tokens.METHOD(!lineRef,!lineRef));
"new"			=> (Tokens.NEW(!lineRef,!lineRef));


{ex}|"_main"  		=> (Tokens.ID(yytext,!lineRef,!lineRef));
_			=> (Tokens.QUOTE(yytext,!lineRef,!lineRef));
