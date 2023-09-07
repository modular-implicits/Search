open Imp.Show;;
open Generic;;


module type Showable = sig 
  type t 
  val showable : t -> string 
end;;

let showable {S : Showable} x = S.showable x;;

implicit module ShowableGenBasic {X : Show} : Showable with type t = X.t genBasic = struct 
    type t = X.t genBasic
    let showable (GenBasic (s, x)) = s ^ " " ^ X.show x
end;;

implicit module ShowableGenProd {X : Showable} {Y : Showable} : Showable with type t = (X.t, Y.t) genProd = struct
  type t = (X.t, Y.t) genProd
  let showable (GenProd (x, y)) = X.showable x ^ ", " ^ Y.showable y
end;;

implicit module ShowableGenSum {X : Showable} {Y : Showable} : Showable with type t = (X.t, Y.t) genSum = struct
  type t = (X.t, Y.t) genSum
  let showable = function
  | (Left a) -> X.showable a
  | (Right b) -> Y.showable b
end;;

implicit module ShowableGeneric {X : Generic} {XRep : Showable with type t = X.rep} : Show with type t = X.t = struct
  type t = X.t
  let show x = XRep.showable (X.toRep x)
end