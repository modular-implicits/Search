

module type Generic = sig
  type t
  type rep
  val toRep : t -> rep
  val fromRep : rep -> t
end;;

let toRep {G : Generic}  = G.toRep;;

let fromRep {G : Generic} = G.fromRep;;

type 'a genBasic = GenBasic of (string * 'a);;

type ('a, 'b) genProd = GenProd of ('a * 'b );;

type ('a, 'b) genSum = Left of 'a | Right of 'b;;






