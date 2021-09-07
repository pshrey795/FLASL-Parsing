structure Token = Tokens
  
    type pos = int
    type svalue = Tokens.svalue
    type ('a,'b) token = ('a,'b) Tokens.token  
    type lexresult = (svalue, pos) token

    exception ScanError

    val rowNum = ref 1;
    val colNum = ref 1;
    val endOfLine = ref 0;

    val eof = fn () => Tokens.EOF(!rowNum, !colNum) 

    val error = fn (e, row:int, col:int) => TextIO.output(TextIO.stdOut,"ScanError:" ^ (Int.toString row) ^ ":" ^ (Int.toString col) ^ ":" ^ e ^ "\n")

    fun atomString([],s,t) = s
    | atomString(head::atomList,s,t) = 
    if head = #"\"" then atomString(atomList,s,false)
    else if head = #" " then (if t then atomString(atomList,s,true) else atomString(atomList,s^" ",true))
    else atomString(atomList,s^Char.toString(head),false)
  

%%

%header (functor flasl2astLexFun(structure Tokens:flasl2ast_TOKENS));
 
ws = [\  \t];
newline = [\r \n \r\n];
atom = [- | ! | #-' | *-, | 0-~ | \ | \t ];
quote = "\"";

%%

{quote}{atom}*{quote}   => (colNum := yypos - !(endOfLine);Token.Atom(atomString(explode yytext,"",false),!rowNum,!colNum));
"NOT"           	=> (colNum := yypos - !(endOfLine);Token.NOT(!rowNum,!colNum));
"AND"           	=> (colNum := yypos - !(endOfLine);Token.AND(!rowNum,!colNum));
"OR"            	=> (colNum := yypos - !(endOfLine);Token.OR(!rowNum,!colNum));
"IF"            	=> (colNum := yypos - !(endOfLine);Token.IF(!rowNum,!colNum));
"THEN"          	=> (colNum := yypos - !(endOfLine);Token.THEN(!rowNum,!colNum));
"ELSE"          	=> (colNum := yypos - !(endOfLine);Token.ELSE(!rowNum,!colNum));
"IFF"               => (colNum := yypos - !(endOfLine);Token.IFF(!rowNum,!colNum));
"THEREFORE"         => (colNum := yypos - !(endOfLine);Token.THEREFORE(!rowNum,!colNum));
"("             	=> (colNum := yypos - !(endOfLine);Token.LPAREN(!rowNum,!colNum));
")"             	=> (colNum := yypos - !(endOfLine);Token.RPAREN(!rowNum,!colNum));
"."                 => (colNum := yypos - !(endOfLine);Token.TERM(!rowNum,!colNum));
{newline}       	=> (rowNum := !rowNum + 1;endOfLine := yypos;lex());
{ws}+           	=> (lex());
.               	=> (colNum := yypos - !(endOfLine)-size yytext;error(yytext,!rowNum,!colNum);raise ScanError);