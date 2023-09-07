open Generic
open Imp.Data

implicit module OrdGenBasic {X : Ord} : Ord with type t = X.t genBasic = struct 
  type t = X.t genBasic
  let compare (GenBasic (_, a)) (GenBasic (_, b)) = X.compare a b
end

implicit module OrdGenProd {X : Ord} {Y : Ord} : Ord with type t = (X.t, Y.t) genProd = struct 
  type t = (X.t, Y.t) genProd
  let compare (GenProd (a1, a2)) (GenProd (b1, b2)) =
                  let comp = X.compare a1 b1 in
                    if comp = EQ then Y.compare a2 b2 else comp
end

implicit module OrdGenSum {X : Ord} {Y : Ord} : Ord with type t = (X.t, Y.t) genSum = struct 
  type t = (X.t, Y.t) genSum
  let compare a b = match a, b with
    | Left _, Right _ -> LT
    | Right _, Left _ -> GT
    | Left a, Left b -> X.compare a b
    | Right a, Right b -> Y.compare a b
end

implicit module OrdGeneric {X : Generic} {XRep : Ord with type t = X.rep} : Ord with type t = X.t = struct 
  type t = X.t
  let compare a b = XRep.compare (toRep a) (toRep b)
end
