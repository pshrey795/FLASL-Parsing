%%

%name flasl2ast

%term
  ATOM of string | NOT | AND | OR | IF | THEN | ELSE | IFF | THEREFORE | TERM | RPAREN | LPAREN | EOF  
%nonterm FILE of ARG.argument | ARGUMENT of ARG.argument | HYP of ARG.hypothesis | PROP of ARG.prop 

%pos int
%eop EOF
%noshift EOF
%start FILE


%right IFF
%nonassoc ELSE
%right THEN
%left AND OR IF
%right NOT

%verbose

%%

FILE: ARGUMENT(ARGUMENT)

ARGUMENT: HYP THEREFORE PROP TERM(ARG.Hence(HYP,PROP))   

HYP: PROP TERM(ARG.addHyp(PROP,ARG.PropList([])))
     | PROP TERM HYP(ARG.addHyp(PROP,HYP))
     | (ARG.PropList([]))

PROP: ATOM(ARG.Atom(ATOM))
     | NOT PROP(ARG.Negation(PROP))
     | PROP AND PROP(ARG.Conjunction(PROP1,PROP2))
     | PROP OR PROP(ARG.Disjunction(PROP1,PROP2))
     | IF PROP THEN PROP(ARG.Conditional(PROP1,PROP2))
     | IF PROP THEN PROP ELSE PROP(ARG.ITE(PROP1,PROP2,PROP3))
     | PROP IF PROP(ARG.Conditional(PROP2,PROP1))
     | PROP IFF PROP(ARG.Biconditional(PROP1,PROP2))
     | LPAREN PROP RPAREN(PROP)		 			