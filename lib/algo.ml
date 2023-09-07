open Imp.Control;;
open Imp.Data;;
open DataStructures
open Imp.Control.Foldable



module DFS (O : Ord) (M : Monad) (Set : St.S with type elt = O.t) = struct 

  let rec dfsM (f : O.t -> (O.t list) M.t) (pred : O.t -> bool M.t)  (start : O.t) (set : Set.t ref) : ((O.t list) option) M.t = 
         (M.bind (pred start) (fun b -> if b then M.return (Some [])
                                                                      else ( if St.mem {Set} start (!set) then M.return None else let () = set := St.add {Set} start (!set) in 
                                                                        M.bind (f start) (
                                                                          fun succs -> (
                                                                            let rec finish = function 
                                                                            | [] -> M.return None
                                                                            | (x :: xs) -> (M.bind (dfsM f pred x set) (
                                                                                             fun res -> match res with
                                                                                             | None -> finish xs
                                                                                             | Some ys -> M.return (Some (x :: ys))
                                                                                           ))
                                                                    in (finish succs)
                                                                          )
                                                                          )
                                                                        )
                                                                      ))



end 

let rec dfsM {O : Ord} {M : Monad} (f : O.t -> (O.t list) M.t) (pred : O.t -> bool M.t) (start : O.t) : ((O.t list) option) M.t = let module X = DFS (O) (M) (St.Make{O}) in  X.dfsM f pred start (ref (St.empty {St.Make{O}}))

let dfs {O : Ord} (f : O.t -> (O.t list)) (pred : O.t -> bool) (start : O.t) : ((O.t list) option) = 
    let f' = fun x -> Identity.return (f x) in
    let pred' = fun x -> Identity.return (pred x) in
    let module X = DFS (O) (Identity) (St.Make{O}) in
    runIdentity (X.dfsM f' pred' start (ref (St.empty {St.Make{O}})))




module DFSF (O : Ord) (M : Monad) (Set : St.S with type elt = O.t) (F : Foldable) = struct 


  let rec dfsM (f : O.t -> (O.t F.t) M.t) (pred : O.t -> bool M.t) (start : O.t) (set : Set.t ref) : ((O.t list) option) M.t = 
         (M.bind (pred start) (fun b -> if b then M.return (Some [])
                                                                      else (if St.mem {Set} start (!set) then M.return None else let () = set := St.add {Set} start (!set) in 
                                                                        M.bind (f start) (
                                                                          fun succs -> (
                                                                          let my_fold (new_op : O.t) (to_return : ((O.t list) option) M.t) : ((O.t list) option) M.t = M.bind to_return ( fun to_return ->
                                                                              match to_return with 
                                                                              | Some xs -> M.return (Some xs)
                                                                              | None -> (M.bind (dfsM f pred new_op set) (
                                                                                             fun res -> match res with
                                                                                             | None -> M.return None
                                                                                             | Some ys -> M.return (Some (new_op :: ys))
                                                                                           )))
                                                                            
                                                                    in (F.fold my_fold succs (M.return None))
                                                                          )
                                                                          )
                                                                        )
                                                                      ))
end 

let rec dfsMF  : {O : Ord} -> {M : Monad} -> {F : Foldable} -> (O.t -> (O.t F.t) M.t) -> (O.t -> bool M.t) -> (O.t) -> ((O.t list) option) M.t = 
        fun {O : Ord} {M : Monad} {F : Foldable} (f : O.t -> (O.t F.t) M.t) (pred : O.t -> bool M.t) (start : O.t) -> let module X = DFSF (O) (M) (St.Make{O}) (F) in  X.dfsM f pred start (ref (St.empty {St.Make{O}}))


let dfsF {O : Ord} {F : Foldable} (f : O.t -> (O.t F.t)) (pred : O.t -> bool) (start : O.t) : ((O.t list) option) = 
      let f' = fun x -> Identity.return (f x) in
      let pred' = fun x -> Identity.return (pred x) in
      let module X = DFSF (O) (Identity) (St.Make{O}) (F) in
      runIdentity (X.dfsM f' pred' start (ref (St.empty {St.Make{O}})))



