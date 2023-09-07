open Generic;;
open Imp.Data;;



implicit module EqGenBasic {X : Eq} : Eq with type t = X.t genBasic = struct 
  type t = X.t genBasic
  let ( = ) (GenBasic (_, a)) (GenBasic (_, b)) = X.( = ) a b
end;;


implicit module EqGenProd {X : Eq} {Y : Eq} : Eq with type t = (X.t, Y.t) genProd = struct 
  type t = (X.t, Y.t) genProd
  let ( = ) (GenProd (a1, a2)) (GenProd (b1, b2)) = X.( = ) a1 b1 && Y.( = ) a2 b2
end;;

implicit module EqGenSum {X : Eq} {Y : Eq} : Eq with type t = (X.t, Y.t) genSum = struct 
  type t = (X.t, Y.t) genSum
  let ( = ) a b = match a, b with
    | Left a, Left b -> X.( = ) a b
    | Right a, Right b -> Y.( = ) a b
    | _ -> false
end;;


implicit module EqGeneric {X : Generic} {XRep : Eq with type t = X.rep} : Eq with type t = X.t = struct 
  type t = X.t
  let ( = ) a b = XRep.(=) (toRep a) (toRep b)
end

