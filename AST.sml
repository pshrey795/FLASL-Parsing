structure AST =
struct

exception Atom_exception

datatype Prop = ATOM of string
                | NOT of Prop
                | AND of Prop * Prop
                | OR of Prop * Prop
                | COND of Prop * Prop
                | BIC of Prop * Prop
                | ITE of Prop * Prop * Prop

datatype Argument = HENCE of Prop list * Prop

fun addHyp(p:Prop, pl:Prop list) = p::pl

end