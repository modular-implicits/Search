open Generic;;


module type Memo = sig 
  type t 
  val memo : (t -> 'a) -> t -> 'a
end;;

let memo {M : Memo} = M.memo;;

implicit module MemoBasic {X : Memo} : Memo with type t = X.t genBasic = struct 
  type t = X.t genBasic
  let memo f = 
        let memoX = X.memo (fun x -> f (GenBasic ("", x))) in
            (fun (GenBasic (_, x)) -> memoX x)
end;;

implicit module MemoisableProd {X : Memo} {Y : Memo} : Memo with type t = (X.t, Y.t) genProd = struct
  type t = (X.t, Y.t) genProd
  let memo f = 
      let memoX = X.memo (fun x -> Y.memo (fun y -> f (GenProd (x, y)))) in 
      fun (GenProd (x, y)) -> memoX x y
end;; 


implicit module MemoisableSum {X : Memo} {Y : Memo} : Memo with type t = (X.t, Y.t) genSum = struct
  type t = (X.t, Y.t) genSum
  let memo f = 
    let memoX = X.memo (fun x -> f (Left x)) in
    let memoY = Y.memo (fun y -> f (Right y)) in
        fun s -> match s  with
            | (Left x) -> memoX x
            | (Right y) -> memoY y
end;;


implicit module MemoisableGeneric {X : Generic} {XRep : Memo with type t = X.rep} : Memo with type t = X.t = struct 
  type t = X.t
  let memo (f : X.t -> 'a) : X.t -> 'a = let memoXRep = XRep.memo (fun x -> f (X.fromRep x)) in 
                  fun x -> memoXRep (X.toRep x)
end;;

module IntMap = Map.Make(struct 
        type t = int
        let compare = compare
end);;

implicit module MemoInt : Memo with type t = int = struct 
    type t = int
    let memo f = 
        let memoMap = ref IntMap.empty in 
        fun x -> 
            if IntMap.mem x !memoMap then 
                IntMap.find x !memoMap
            else 
                let y = f x in 
                memoMap := IntMap.add x y !memoMap;
                y
end;;



module StrMap = Map.Make(struct 
        type t = string
        let compare = compare
end);;

implicit module MemoStr : Memo with type t = string = struct 
    type t = string
    let memo f = 
        let memoMap = ref StrMap.empty in 
        fun x -> 
            if StrMap.mem x !memoMap then 
                StrMap.find x !memoMap
            else 
                let y = f x in 
                memoMap := StrMap.add x y !memoMap;
                y
end;;
