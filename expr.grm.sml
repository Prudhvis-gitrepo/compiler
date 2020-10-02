functor ExprLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Expr_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "expr.grm"*)
(*#line 12.1 "expr.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\025\000\009\000\024\000\011\000\023\000\015\000\022\000\
\\018\000\021\000\020\000\020\000\022\000\019\000\023\000\018\000\
\\037\000\011\000\048\000\010\000\049\000\009\000\050\000\008\000\000\000\
\\001\000\002\000\202\000\005\000\202\000\007\000\202\000\010\000\202\000\
\\016\000\202\000\017\000\202\000\019\000\202\000\021\000\202\000\
\\024\000\202\000\025\000\202\000\026\000\202\000\027\000\202\000\
\\028\000\202\000\030\000\202\000\031\000\202\000\032\000\202\000\
\\033\000\202\000\034\000\202\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\045\000\202\000\046\000\202\000\
\\047\000\202\000\000\000\
\\001\000\002\000\203\000\005\000\203\000\007\000\203\000\010\000\203\000\
\\016\000\203\000\017\000\203\000\019\000\203\000\021\000\203\000\
\\024\000\203\000\025\000\203\000\026\000\203\000\027\000\203\000\
\\028\000\203\000\030\000\203\000\031\000\203\000\032\000\203\000\
\\033\000\203\000\034\000\203\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\045\000\203\000\046\000\203\000\
\\047\000\203\000\000\000\
\\001\000\002\000\204\000\005\000\204\000\007\000\204\000\010\000\204\000\
\\016\000\204\000\017\000\204\000\019\000\204\000\021\000\204\000\
\\024\000\204\000\025\000\204\000\026\000\204\000\027\000\204\000\
\\028\000\204\000\030\000\204\000\031\000\204\000\032\000\204\000\
\\033\000\204\000\034\000\204\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\045\000\204\000\046\000\204\000\
\\047\000\204\000\000\000\
\\001\000\002\000\205\000\005\000\205\000\007\000\205\000\010\000\205\000\
\\016\000\205\000\017\000\205\000\019\000\205\000\021\000\205\000\
\\024\000\205\000\025\000\205\000\026\000\205\000\027\000\205\000\
\\028\000\205\000\030\000\205\000\031\000\205\000\032\000\205\000\
\\033\000\205\000\034\000\205\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\045\000\205\000\046\000\205\000\
\\047\000\205\000\000\000\
\\001\000\002\000\206\000\005\000\206\000\007\000\206\000\010\000\206\000\
\\016\000\206\000\017\000\206\000\019\000\206\000\021\000\206\000\
\\024\000\206\000\025\000\206\000\026\000\206\000\027\000\206\000\
\\028\000\206\000\030\000\206\000\031\000\206\000\032\000\206\000\
\\033\000\206\000\034\000\206\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\045\000\206\000\046\000\206\000\
\\047\000\206\000\000\000\
\\001\000\002\000\207\000\005\000\207\000\007\000\207\000\010\000\207\000\
\\016\000\207\000\017\000\207\000\019\000\207\000\021\000\207\000\
\\024\000\207\000\025\000\207\000\026\000\207\000\027\000\207\000\
\\028\000\207\000\030\000\207\000\031\000\207\000\032\000\207\000\
\\033\000\207\000\034\000\207\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\045\000\207\000\046\000\207\000\
\\047\000\207\000\000\000\
\\001\000\002\000\094\000\008\000\041\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\001\000\002\000\100\000\008\000\041\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\001\000\004\000\133\000\000\000\
\\001\000\005\000\098\000\000\000\
\\001\000\005\000\131\000\000\000\
\\001\000\005\000\157\000\000\000\
\\001\000\005\000\167\000\000\000\
\\001\000\005\000\170\000\000\000\
\\001\000\005\000\184\000\000\000\
\\001\000\006\000\084\000\029\000\083\000\000\000\
\\001\000\006\000\115\000\028\000\114\000\035\000\113\000\048\000\112\000\000\000\
\\001\000\006\000\129\000\000\000\
\\001\000\006\000\135\000\029\000\134\000\000\000\
\\001\000\006\000\169\000\000\000\
\\001\000\008\000\041\000\016\000\089\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\001\000\008\000\041\000\019\000\088\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\001\000\008\000\041\000\019\000\171\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\001\000\008\000\041\000\021\000\138\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\001\000\008\000\085\000\000\000\
\\001\000\008\000\099\000\000\000\
\\001\000\008\000\151\000\012\000\150\000\000\000\
\\001\000\008\000\173\000\000\000\
\\001\000\008\000\175\000\000\000\
\\001\000\008\000\183\000\012\000\182\000\000\000\
\\001\000\008\000\190\000\000\000\
\\001\000\010\000\090\000\000\000\
\\001\000\010\000\095\000\000\000\
\\001\000\010\000\126\000\000\000\
\\001\000\010\000\128\000\000\000\
\\001\000\010\000\141\000\000\000\
\\001\000\010\000\176\000\000\000\
\\001\000\011\000\081\000\000\000\
\\001\000\011\000\082\000\000\000\
\\001\000\011\000\153\000\000\000\
\\001\000\012\000\080\000\014\000\079\000\000\000\
\\001\000\012\000\127\000\000\000\
\\001\000\012\000\180\000\000\000\
\\001\000\014\000\087\000\000\000\
\\001\000\014\000\125\000\000\000\
\\001\000\024\000\086\000\000\000\
\\001\000\025\000\137\000\000\000\
\\001\000\047\000\000\000\000\000\
\\001\000\048\000\046\000\000\000\
\\001\000\048\000\048\000\000\000\
\\001\000\048\000\049\000\000\000\
\\001\000\048\000\050\000\000\000\
\\001\000\048\000\051\000\000\000\
\\001\000\048\000\053\000\000\000\
\\001\000\048\000\058\000\000\000\
\\001\000\048\000\060\000\000\000\
\\001\000\048\000\102\000\000\000\
\\001\000\048\000\106\000\000\000\
\\001\000\048\000\132\000\000\000\
\\001\000\048\000\149\000\000\000\
\\001\000\048\000\154\000\000\000\
\\001\000\048\000\155\000\000\000\
\\001\000\048\000\161\000\000\000\
\\001\000\048\000\162\000\000\000\
\\001\000\048\000\165\000\000\000\
\\001\000\048\000\174\000\000\000\
\\001\000\048\000\186\000\000\000\
\\001\000\048\000\187\000\000\000\
\\001\000\049\000\047\000\000\000\
\\193\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\038\000\038\000\039\000\037\000\000\000\
\\199\000\038\000\038\000\039\000\037\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\208\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\000\000\
\\209\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\000\000\
\\210\000\038\000\038\000\039\000\037\000\000\000\
\\211\000\008\000\041\000\017\000\139\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\212\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\213\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\214\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\003\000\029\000\013\000\028\000\014\000\027\000\000\000\
\\223\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\224\000\000\000\
\\225\000\001\000\025\000\009\000\024\000\011\000\023\000\015\000\022\000\
\\018\000\021\000\020\000\020\000\022\000\019\000\023\000\018\000\
\\037\000\011\000\048\000\010\000\049\000\009\000\050\000\008\000\000\000\
\\226\000\000\000\
\\227\000\008\000\041\000\026\000\092\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\228\000\000\000\
\\229\000\048\000\077\000\000\000\
\\230\000\000\000\
\\231\000\007\000\146\000\008\000\041\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\232\000\000\000\
\\233\000\001\000\025\000\009\000\024\000\011\000\023\000\015\000\022\000\
\\018\000\021\000\020\000\020\000\022\000\019\000\023\000\018\000\
\\037\000\011\000\048\000\010\000\049\000\009\000\050\000\008\000\000\000\
\\234\000\000\000\
\\235\000\007\000\097\000\008\000\041\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\236\000\000\000\
\\237\000\003\000\044\000\006\000\043\000\011\000\042\000\000\000\
\\238\000\011\000\093\000\000\000\
\\239\000\000\000\
\\240\000\001\000\025\000\009\000\024\000\011\000\023\000\015\000\022\000\
\\018\000\021\000\020\000\020\000\022\000\019\000\023\000\018\000\
\\037\000\011\000\048\000\010\000\049\000\009\000\050\000\008\000\000\000\
\\241\000\000\000\
\\242\000\007\000\143\000\008\000\041\000\036\000\040\000\037\000\039\000\
\\038\000\038\000\039\000\037\000\040\000\036\000\041\000\035\000\
\\042\000\034\000\043\000\033\000\044\000\032\000\045\000\031\000\
\\046\000\030\000\000\000\
\\243\000\000\000\
\\244\000\001\000\025\000\009\000\024\000\011\000\023\000\015\000\022\000\
\\018\000\021\000\020\000\020\000\022\000\019\000\023\000\018\000\
\\027\000\017\000\028\000\016\000\030\000\015\000\031\000\014\000\
\\032\000\013\000\033\000\012\000\037\000\011\000\048\000\010\000\
\\049\000\009\000\050\000\008\000\000\000\
\\244\000\027\000\017\000\028\000\016\000\030\000\015\000\031\000\014\000\
\\032\000\013\000\033\000\012\000\000\000\
\\245\000\000\000\
\\246\000\000\000\
\\247\000\000\000\
\\248\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\249\000\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\250\000\012\000\148\000\000\000\
\\251\000\000\000\
\\252\000\000\000\
\\253\000\000\000\
\\254\000\000\000\
\\255\000\000\000\
\\000\001\000\000\
\\001\001\000\000\
\\002\001\000\000\
\\003\001\000\000\
\\004\001\048\000\104\000\000\000\
\\005\001\000\000\
\\006\001\007\000\164\000\000\000\
\\007\001\000\000\
\\008\001\033\000\012\000\034\000\110\000\000\000\
\\009\001\000\000\
\\010\001\000\000\
\\011\001\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\012\001\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\013\001\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\\014\001\008\000\041\000\036\000\040\000\037\000\039\000\038\000\038\000\
\\039\000\037\000\040\000\036\000\041\000\035\000\042\000\034\000\
\\043\000\033\000\044\000\032\000\045\000\031\000\046\000\030\000\000\000\
\"
val actionRowNumbers =
"\115\000\126\000\116\000\071\000\
\\093\000\070\000\073\000\074\000\
\\108\000\000\000\049\000\069\000\
\\050\000\051\000\052\000\053\000\
\\116\000\086\000\054\000\000\000\
\\000\000\096\000\055\000\072\000\
\\117\000\000\000\056\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\104\000\100\000\000\000\081\000\
\\041\000\118\000\038\000\039\000\
\\016\000\025\000\046\000\044\000\
\\022\000\021\000\032\000\098\000\
\\091\000\094\000\109\000\007\000\
\\080\000\079\000\006\000\005\000\
\\004\000\003\000\002\000\078\000\
\\077\000\076\000\075\000\001\000\
\\033\000\106\000\010\000\026\000\
\\008\000\000\000\057\000\132\000\
\\132\000\058\000\136\000\017\000\
\\096\000\000\000\000\000\000\000\
\\088\000\097\000\000\000\111\000\
\\110\000\092\000\105\000\000\000\
\\090\000\000\000\089\000\142\000\
\\045\000\034\000\042\000\035\000\
\\018\000\138\000\136\000\011\000\
\\059\000\119\000\127\000\009\000\
\\019\000\132\000\047\000\024\000\
\\084\000\082\000\098\000\036\000\
\\113\000\106\000\102\000\000\000\
\\122\000\060\000\027\000\136\000\
\\137\000\124\000\040\000\061\000\
\\062\000\136\000\012\000\087\000\
\\000\000\000\000\099\000\095\000\
\\112\000\000\000\107\000\101\000\
\\063\000\141\000\064\000\134\000\
\\065\000\000\000\013\000\132\000\
\\129\000\020\000\014\000\128\000\
\\023\000\083\000\113\000\028\000\
\\123\000\133\000\066\000\029\000\
\\120\000\125\000\037\000\136\000\
\\130\000\000\000\114\000\000\000\
\\043\000\000\000\030\000\015\000\
\\085\000\102\000\067\000\121\000\
\\068\000\000\000\131\000\103\000\
\\134\000\031\000\139\000\135\000\
\\000\000\140\000\048\000"
val gotoT =
"\
\\001\000\190\000\002\000\005\000\009\000\004\000\012\000\003\000\
\\013\000\002\000\019\000\001\000\000\000\
\\000\000\
\\012\000\024\000\013\000\002\000\019\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\043\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\050\000\013\000\002\000\019\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\052\000\009\000\004\000\000\000\
\\002\000\053\000\009\000\004\000\000\000\
\\002\000\055\000\003\000\054\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\057\000\009\000\004\000\000\000\
\\000\000\
\\002\000\059\000\009\000\004\000\000\000\
\\002\000\060\000\009\000\004\000\000\000\
\\002\000\061\000\009\000\004\000\000\000\
\\002\000\062\000\009\000\004\000\000\000\
\\002\000\063\000\009\000\004\000\000\000\
\\002\000\064\000\009\000\004\000\000\000\
\\002\000\065\000\009\000\004\000\000\000\
\\002\000\066\000\009\000\004\000\000\000\
\\002\000\067\000\009\000\004\000\000\000\
\\002\000\068\000\009\000\004\000\000\000\
\\002\000\069\000\009\000\004\000\000\000\
\\002\000\070\000\009\000\004\000\000\000\
\\002\000\071\000\009\000\004\000\000\000\
\\002\000\073\000\007\000\072\000\009\000\004\000\000\000\
\\005\000\074\000\000\000\
\\002\000\076\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\089\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\099\000\009\000\004\000\000\000\
\\000\000\
\\015\000\101\000\000\000\
\\015\000\103\000\000\000\
\\000\000\
\\017\000\107\000\018\000\106\000\019\000\105\000\000\000\
\\014\000\109\000\000\000\
\\002\000\055\000\003\000\114\000\009\000\004\000\000\000\
\\002\000\115\000\009\000\004\000\000\000\
\\002\000\116\000\009\000\004\000\000\000\
\\002\000\117\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\002\000\118\000\009\000\004\000\000\000\
\\002\000\120\000\009\000\004\000\010\000\119\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\121\000\009\000\004\000\000\000\
\\000\000\
\\002\000\122\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\128\000\018\000\106\000\019\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\134\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\138\000\000\000\
\\000\000\
\\011\000\140\000\000\000\
\\008\000\142\000\000\000\
\\006\000\143\000\000\000\
\\002\000\145\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\150\000\018\000\106\000\019\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\154\000\018\000\106\000\019\000\105\000\000\000\
\\000\000\
\\000\000\
\\002\000\156\000\009\000\004\000\000\000\
\\002\000\157\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\158\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\161\000\000\000\
\\000\000\
\\002\000\164\000\009\000\004\000\000\000\
\\000\000\
\\015\000\166\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\170\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\175\000\018\000\106\000\019\000\105\000\000\000\
\\000\000\
\\002\000\176\000\009\000\004\000\000\000\
\\000\000\
\\002\000\177\000\009\000\004\000\000\000\
\\000\000\
\\002\000\179\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\183\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\186\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\\016\000\187\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\189\000\009\000\004\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 191
val numrules = 78
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | CONST of  (int) | QUOTE of  (string) | ID of  (string) | VARDEC of  (Ast.VarDec) | CLASSFIELD of  (Ast.ClassField) | CLASSFIELDS of  (Ast.ClassField list) | TYFILEDStail of  ( ( Ast.Id * Ast.Id )  list) | TYFIELDS of  ( ( Ast.Id * Ast.Id )  list) | TY of  (Ast.Type) | DEC of  (Ast.DEC) | DECS of  (Ast.DEC list) | METHODtail of  (Ast.EXP list) | METHODBODY of  (Ast.EXP list) | LVALUE of  (Ast.EXP) | FUNCTIONTAIL of  (Ast.EXP list) | FUNCTIONBODY of  ( ( Ast.EXP )  list) | RECTAIL of  ( ( Ast.Id * Ast.EXP )  list) | RECBODY of  ( ( Ast.Id * Ast.EXP )  list) | EXPtail of  (Ast.EXP list) | EXPS of  (Ast.EXP list) | EXP of  (Ast.EXP) | PROGRAM of  (Ast.Program)
end
type svalue = MlyValue.svalue
type result = Ast.Program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 46) => true | _ => false
val showTerminal =
fn (T 0) => "NIL"
  | (T 1) => "RB"
  | (T 2) => "LB"
  | (T 3) => "OF"
  | (T 4) => "RCB"
  | (T 5) => "LCB"
  | (T 6) => "COMMA"
  | (T 7) => "EQUAL"
  | (T 8) => "NEW"
  | (T 9) => "RP"
  | (T 10) => "LP"
  | (T 11) => "COLON"
  | (T 12) => "DOT"
  | (T 13) => "ASSIGN"
  | (T 14) => "IF"
  | (T 15) => "THEN"
  | (T 16) => "ELSE"
  | (T 17) => "WHILE"
  | (T 18) => "DO"
  | (T 19) => "FOR"
  | (T 20) => "TO"
  | (T 21) => "BREAK"
  | (T 22) => "LET"
  | (T 23) => "IN"
  | (T 24) => "END"
  | (T 25) => "SEMICOLON"
  | (T 26) => "TYPE"
  | (T 27) => "CLASS"
  | (T 28) => "EXTENDS"
  | (T 29) => "FUNCTION"
  | (T 30) => "PRIMITIVE"
  | (T 31) => "IMPORT"
  | (T 32) => "VAR"
  | (T 33) => "METHOD"
  | (T 34) => "ARRAY"
  | (T 35) => "PLUS"
  | (T 36) => "MINUS"
  | (T 37) => "MUL"
  | (T 38) => "DIV"
  | (T 39) => "LESS"
  | (T 40) => "GREAT"
  | (T 41) => "LESSGREAT"
  | (T 42) => "LESSEQUAL"
  | (T 43) => "GREATEQUAL"
  | (T 44) => "AND"
  | (T 45) => "OR"
  | (T 46) => "EOF"
  | (T 47) => "ID"
  | (T 48) => "QUOTE"
  | (T 49) => "CONST"
  | (T 50) => "UMINUS"
  | (T 51) => "lv"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 51) $$ (T 50) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 56.18 "expr.grm"*)Ast.Expression EXP(*#line 662.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.DECS DECS, DECS1left, DECS1right)) :: rest671)) => let val  result = MlyValue.PROGRAM ((*#line 57.12 "expr.grm"*)Ast.Decls DECS(*#line 666.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, DECS1left, DECS1right), rest671)
end
|  ( 2, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 59.13 "expr.grm"*)Ast.Nil(*#line 670.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.CONST CONST, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 60.12 "expr.grm"*)Ast.Const CONST (*#line 674.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, CONST1left, CONST1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.QUOTE QUOTE, QUOTE1left, QUOTE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 61.12 "expr.grm"*)Ast.Quote QUOTE (*#line 678.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, QUOTE1left, QUOTE1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 62.19 "expr.grm"*)Ast.plus EXP1 EXP2(*#line 682.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 63.20 "expr.grm"*)Ast.minus EXP1 EXP2(*#line 686.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 64.18 "expr.grm"*)Ast.mul EXP1 EXP2(*#line 690.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 65.17 "expr.grm"*)Ast.divide EXP1 EXP2(*#line 694.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 66.20 "expr.grm"*)Ast.equal EXP1 EXP2(*#line 698.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 67.19 "expr.grm"*)Ast.less EXP1 EXP2(*#line 702.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 68.20 "expr.grm"*)Ast.great EXP1 EXP2(*#line 706.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 69.23 "expr.grm"*)Ast.lessgreat EXP1 EXP2(*#line 710.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 70.23 "expr.grm"*)Ast.lessequal EXP1 EXP2(*#line 714.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 71.24 "expr.grm"*)Ast.greatequal EXP1 EXP2(*#line 718.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 72.18 "expr.grm"*)Ast.And EXP1 EXP2(*#line 722.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 73.17 "expr.grm"*)Ast.or EXP1 EXP2(*#line 726.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, MINUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 74.16 "expr.grm"*)Ast.UMINUS EXP(*#line 730.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, MINUS1left, EXP1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 75.31 "expr.grm"*)Ast.If EXP1 EXP2(*#line 734.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, IF1left, EXP2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 76.42 "expr.grm"*)Ast.Else EXP1 EXP2 EXP3(*#line 738.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, IF1left, EXP3right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 77.35 "expr.grm"*)Ast.While EXP1 EXP2(*#line 742.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 78.50 "expr.grm"*)Ast.For (Ast.Var ID) EXP1 EXP2 EXP3 (*#line 746.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, FOR1left, EXP3right), rest671)
end
|  ( 22, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 79.13 "expr.grm"*)Ast.Break(*#line 750.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 23, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: _ :: ( _, ( MlyValue.DECS DECS, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 80.26 "expr.grm"*)Ast.Let (DECS,EXPS)(*#line 754.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RP1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: ( _, ( _, LP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 81.17 "expr.grm"*)Ast.Exps EXPS(*#line 758.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, LP1left, RP1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RB1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 82.28 "expr.grm"*)Ast.ArrayCreate (( Ast.Var ID),EXP1 )(*#line 762.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, ID1left, RB1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.RECBODY RECBODY, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 83.24 "expr.grm"*)Ast.RecordCreate (( Ast.Var ID),RECBODY)(*#line 766.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, ID1left, RCB1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: ( _, ( _, NEW1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 84.14 "expr.grm"*)Ast.New (Ast.Var ID)(*#line 770.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, NEW1left, ID1right), rest671)
end
|  ( 28, ( ( _, ( _, _, RP1right)) :: ( _, ( MlyValue.FUNCTIONBODY FUNCTIONBODY, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 85.27 "expr.grm"*)Ast.Functioncall ((Ast.Var ID),FUNCTIONBODY)(*#line 774.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, ID1left, RP1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 86.13 "expr.grm"*)LVALUE(*#line 778.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 87.36 "expr.grm"*)Ast.Assign (LVALUE,EXP) (*#line 782.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, LVALUE1left, EXP1right), rest671)
end
|  ( 31, ( ( _, ( _, _, RP1right)) :: ( _, ( MlyValue.METHODBODY METHODBODY, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 88.36 "expr.grm"*)Ast.Methodcall (LVALUE,(Ast.Var ID),METHODBODY) (*#line 786.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, LVALUE1left, RP1right), rest671)
end
|  ( 32, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 90.13 "expr.grm"*) [] (*#line 790.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 33, ( ( _, ( MlyValue.EXPtail EXPtail, _, EXPtail1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 91.19 "expr.grm"*) EXP :: EXPtail (*#line 794.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXPtail1right), rest671)
end
|  ( 34, ( rest671)) => let val  result = MlyValue.EXPtail ((*#line 93.15 "expr.grm"*) [] (*#line 798.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 35, ( ( _, ( MlyValue.EXPtail EXPtail, _, EXPtail1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, SEMICOLON1left, _)) :: rest671)) => let val  result = MlyValue.EXPtail ((*#line 94.27 "expr.grm"*) EXP :: EXPtail(*#line 802.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, SEMICOLON1left, EXPtail1right), rest671)
end
|  ( 36, ( rest671)) => let val  result = MlyValue.RECBODY ((*#line 96.14 "expr.grm"*) [] (*#line 806.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 37, ( ( _, ( MlyValue.RECTAIL RECTAIL, _, RECTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.RECBODY ((*#line 97.27 "expr.grm"*) ((Ast.Var ID),EXP) :: RECTAIL (*#line 810.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, RECTAIL1right), rest671)
end
|  ( 38, ( rest671)) => let val  result = MlyValue.RECTAIL ((*#line 99.16 "expr.grm"*) [] (*#line 814.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 39, ( ( _, ( MlyValue.RECTAIL RECTAIL, _, RECTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.RECTAIL ((*#line 100.32 "expr.grm"*) ((Ast.Var ID),EXP) :: RECTAIL (*#line 818.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, COMMA1left, RECTAIL1right), rest671)
end
|  ( 40, ( rest671)) => let val  result = MlyValue.FUNCTIONBODY ((*#line 102.19 "expr.grm"*) [] (*#line 822.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 41, ( ( _, ( MlyValue.FUNCTIONTAIL FUNCTIONTAIL, _, FUNCTIONTAIL1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.FUNCTIONBODY ((*#line 103.22 "expr.grm"*) EXP :: FUNCTIONTAIL(*#line 826.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, EXP1left, FUNCTIONTAIL1right), rest671)
end
|  ( 42, ( rest671)) => let val  result = MlyValue.FUNCTIONTAIL ((*#line 105.20 "expr.grm"*) [] (*#line 830.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( MlyValue.FUNCTIONTAIL FUNCTIONTAIL, _, FUNCTIONTAIL1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.FUNCTIONTAIL ((*#line 106.28 "expr.grm"*) EXP ::FUNCTIONTAIL(*#line 834.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, COMMA1left, FUNCTIONTAIL1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 108.23 "expr.grm"*)Ast.IDENT(Ast.Var ID)(*#line 838.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 109.20 "expr.grm"*)Ast.Lvalue1 (LVALUE, (Ast.Var ID))(*#line 842.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, LVALUE1left, ID1right), rest671)
end
|  ( 46, ( ( _, ( _, _, RB1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 110.22 "expr.grm"*)Ast.Lvalue2 (LVALUE,EXP) (*#line 846.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, LVALUE1left, RB1right), rest671)
end
|  ( 47, ( rest671)) => let val  result = MlyValue.METHODBODY ((*#line 112.17 "expr.grm"*) [] (*#line 850.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 48, ( ( _, ( MlyValue.METHODtail METHODtail, _, METHODtail1right)) :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.METHODBODY ((*#line 113.20 "expr.grm"*) EXP :: METHODtail(*#line 854.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, EXP1left, METHODtail1right), rest671)
end
|  ( 49, ( rest671)) => let val  result = MlyValue.METHODtail ((*#line 115.17 "expr.grm"*) [] (*#line 858.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 50, ( ( _, ( MlyValue.METHODtail METHODtail, _, METHODtail1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.METHODtail ((*#line 116.28 "expr.grm"*) EXP :: METHODtail(*#line 862.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, COMMA1left, METHODtail1right), rest671)
end
|  ( 51, ( rest671)) => let val  result = MlyValue.DECS ((*#line 118.12 "expr.grm"*) [] (*#line 866.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 52, ( ( _, ( MlyValue.DECS DECS, _, DECS1right)) :: ( _, ( MlyValue.DEC DEC, DEC1left, _)) :: rest671)) => let val  result = MlyValue.DECS ((*#line 119.15 "expr.grm"*) DEC :: DECS(*#line 870.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, DEC1left, DECS1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.QUOTE QUOTE, _, QUOTE1right)) :: ( _, ( _, IMPORT1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 121.22 "expr.grm"*)Ast.Import (Ast.Quote QUOTE) (*#line 874.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, IMPORT1left, QUOTE1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.TY TY, _, TY1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 122.22 "expr.grm"*) Ast.Typedec (Ast.Var ID,TY) (*#line 878.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, TYPE1left, TY1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 123.42 "expr.grm"*) Ast.Functiondec1 ( (Ast.Var ID ),Ast.Tyfields TYFIELDS  ,EXP ) (*#line 882.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 124.51 "expr.grm"*) Ast.Functiondec2 ( (Ast.Var ID1),(Ast.Var ID2),Ast.Tyfields TYFIELDS  ,EXP ) (*#line 886.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, FUNCTION1left, EXP1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RP1right)) :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, PRIMITIVE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 125.34 "expr.grm"*) Ast.Primitivedec1 ( (Ast.Var ID ),Ast.Tyfields TYFIELDS ) (*#line 890.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, PRIMITIVE1left, RP1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, PRIMITIVE1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 126.43 "expr.grm"*) Ast.Primitivedec2 ( (Ast.Var ID1),(Ast.Var ID2),Ast.Tyfields TYFIELDS )  (*#line 894.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, PRIMITIVE1left, ID2right), rest671)
end
|  ( 59, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, CLASS1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 127.34 "expr.grm"*)Ast.Classdec1 ((Ast.Var ID),CLASSFIELDS)(*#line 898.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, CLASS1left, RCB1right), rest671)
end
|  ( 60, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, CLASS1left, _)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 128.45 "expr.grm"*)Ast.Classdec2 ((Ast.Var ID1),(Ast.Var ID2),CLASSFIELDS)(*#line 902.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, CLASS1left, RCB1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.VARDEC VARDEC, VARDEC1left, VARDEC1right)) :: rest671)) => let val  result = MlyValue.DEC ((*#line 129.13 "expr.grm"*)Ast.Vardec VARDEC (*#line 906.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, VARDEC1left, VARDEC1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.TY ((*#line 132.12 "expr.grm"*) Ast.TyIDENT ( Ast.Var ID)(*#line 910.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, ID1left, ID1right), rest671)
end
|  ( 63, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: ( _, ( _, LCB1left, _)) :: rest671)) => let val  result = MlyValue.TY ((*#line 133.22 "expr.grm"*) Ast.Tyf (Ast.Tyfields TYFIELDS) (*#line 914.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, LCB1left, RCB1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: _ :: ( _, ( _, ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.TY ((*#line 134.18 "expr.grm"*) Ast.Array (Ast.Var ID)(*#line 918.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 65, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, _)) :: _ :: ( _, ( _, CLASS1left, _)) :: rest671)) => let val  result = MlyValue.TY ((*#line 135.31 "expr.grm"*) Ast.Classfields CLASSFIELDS (*#line 922.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, CLASS1left, RCB1right), rest671)
end
|  ( 66, ( ( _, ( _, _, RCB1right)) :: ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( _, CLASS1left, _)) :: rest671)) => let val  result = MlyValue.TY ((*#line 136.42 "expr.grm"*) Ast.Classfields2 ((Ast.Var ID),CLASSFIELDS)(*#line 926.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, CLASS1left, RCB1right), rest671)
end
|  ( 67, ( rest671)) => let val  result = MlyValue.TYFIELDS ((*#line 138.16 "expr.grm"*) [] (*#line 930.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 68, ( ( _, ( MlyValue.TYFILEDStail TYFILEDStail, _, TYFILEDStail1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.TYFIELDS ((*#line 139.30 "expr.grm"*)( Ast.Var ID1, Ast.Var ID2 ) :: TYFILEDStail (*#line 934.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, ID1left, TYFILEDStail1right), rest671)
end
|  ( 69, ( rest671)) => let val  result = MlyValue.TYFILEDStail ((*#line 141.20 "expr.grm"*) [] (*#line 938.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 70, ( ( _, ( MlyValue.TYFILEDStail TYFILEDStail, _, TYFILEDStail1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = MlyValue.TYFILEDStail ((*#line 142.36 "expr.grm"*) ( Ast.Var ID1,Ast.Var ID2 ) :: TYFILEDStail (*#line 942.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, COMMA1left, TYFILEDStail1right), rest671)
end
|  ( 71, ( rest671)) => let val  result = MlyValue.CLASSFIELDS ((*#line 144.19 "expr.grm"*) [] (*#line 946.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, defaultPos, defaultPos), rest671)
end
|  ( 72, ( ( _, ( MlyValue.CLASSFIELDS CLASSFIELDS, _, CLASSFIELDS1right)) :: ( _, ( MlyValue.CLASSFIELD CLASSFIELD, CLASSFIELD1left, _)) :: rest671)) => let val  result = MlyValue.CLASSFIELDS ((*#line 145.28 "expr.grm"*) CLASSFIELD :: CLASSFIELDS(*#line 950.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, CLASSFIELD1left, CLASSFIELDS1right), rest671)
end
|  ( 73, ( ( _, ( MlyValue.VARDEC VARDEC, VARDEC1left, VARDEC1right)) :: rest671)) => let val  result = MlyValue.CLASSFIELD ((*#line 147.21 "expr.grm"*) Ast.ClassVar VARDEC (*#line 954.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, VARDEC1left, VARDEC1right), rest671)
end
|  ( 74, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, METHOD1left, _)) :: rest671)) => let val  result = MlyValue.CLASSFIELD ((*#line 148.40 "expr.grm"*) Ast.Method1 ( (Ast.Var ID), (Ast.Tyfields TYFIELDS), EXP)(*#line 958.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, METHOD1left, EXP1right), rest671)
end
|  ( 75, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.TYFIELDS TYFIELDS, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, METHOD1left, _)) :: rest671)) => let val  result = MlyValue.CLASSFIELD ((*#line 149.49 "expr.grm"*) Ast.Method2 ( (Ast.Var ID1),(Ast.Var ID2),(Ast.Tyfields TYFIELDS), EXP)(*#line 962.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, METHOD1left, EXP1right), rest671)
end
|  ( 76, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDEC ((*#line 151.39 "expr.grm"*) Ast.Vardec1 ((Ast.Var ID1),(Ast.Var ID2),EXP) (*#line 966.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, VAR1left, EXP1right), rest671)
end
|  ( 77, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.VARDEC ((*#line 152.24 "expr.grm"*) Ast.Vardec2 ((Ast.Var ID1),EXP) (*#line 970.1 "expr.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, VAR1left, EXP1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROGRAM x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Expr_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun RB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun LB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun RCB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun LCB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun NEW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun RP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun LP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun CLASS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun EXTENDS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun PRIMITIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun IMPORT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun METHOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
fun GREAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID,p1,p2))
fun LESSGREAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID,p1,p2))
fun LESSEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(ParserData.MlyValue.VOID,p1,p2))
fun GREATEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(ParserData.MlyValue.ID i,p1,p2))
fun QUOTE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(ParserData.MlyValue.QUOTE i,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(ParserData.MlyValue.CONST i,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(ParserData.MlyValue.VOID,p1,p2))
fun lv (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(ParserData.MlyValue.VOID,p1,p2))
end
end
