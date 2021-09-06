structure flasl2astLrVals = flasl2astLrValsFun(structure Token = LrParser.Token)
structure flasl2astLex = flasl2astLexFun(structure Tokens = flasl2astLrVals.Tokens);
structure flasl2astParser =	Join(structure LrParser = LrParser
                    structure ParserData = flasl2astLrVals.ParserData
     	       		structure Lex = flasl2astLex)

fun fileToLexer fileName =
let
    val instream = TextIO.openIn fileName
    val inputString = TextIO.input instream
    val _ = TextIO.closeIn instream
    val done = ref false
    val lexer = flasl2astParser.makeLexer(fn _ => if !done then "" else (done := true;inputString))
in
    lexer
end

fun runLexer lexer = 
let
    fun print_error(s,rowNum,colNum) = TextIO.output(TextIO.stdOut, "Error at "^(Int.toString rowNum)^" "^(Int.toString colNum)^"\n")
in
    flasl2astParser.parse(0,lexer,print_error,())
end
