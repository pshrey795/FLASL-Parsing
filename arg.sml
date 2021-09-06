structure ARG =
struct

datatype prop = Atom of string
                | Negation of prop
                | Conjunction of prop * prop
                | Disjunction of prop * prop
                | Conditional of prop * prop
                | Biconditional of prop * prop
                | ITE of prop * prop * prop

datatype hypothesis = PropList of prop list 

datatype argument = Hence of hypothesis * prop

fun addHyp(p:prop, h:hypothesis) = case h of PropList(ls) => PropList(p::ls)

end