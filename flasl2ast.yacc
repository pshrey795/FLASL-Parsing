%%

%name flasl2ast

%term
  ATOM of string | NOT | AND | OR | IF | THEN | ELSE | IFF | THEREFORE | TERM | RPAREN | LPAREN | EOF  

%nonterm ARGUMENT | HYP | PROP 

%pos int

%eop EOF
%noshift EOF

%start ARGUMENT

(* Defining associativity rules *)
%right IFF
%left AND OR IF
%right NOT
%nonassoc LPAREN

%verbose

%%

(* Production Rules along with printing them *)

ARGUMENT: HYP THEREFORE PROP TERM()   

HYP: PROP TERM()
     | HYP PROP TERM()

PROP: ATOM()
     | NOT PROP()
     | PROP AND PROP()
     | PROP OR PROP()
     | LPAREN IF PROP THEN PROP RPAREN()
     | LPAREN IF PROP THEN PROP ELSE PROP RPAREN()
     | PROP IF PROP()
     | PROP IFF PROP()
     | LPAREN PROP RPAREN()		 			