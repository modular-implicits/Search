open Imp.Control;;
open Imp.Data;;
open DataStructures




let rec dfsM' : ({O : Ord} -> {M : Monad} -> {Set : St.S with type elt = O.t} -> (O.t -> (O.t list) M.t) -> (O.t -> bool M.t) -> O.t -> Set.t -> ((O.t list) option) M.t) = 
  fun {O : Ord} {M : Monad} {Set : St.S with type elt = O.t} (f : O.t -> (O.t list) M.t) (pred : O.t -> bool M.t)  (start : O.t) (set : Set.t) ->
       let set' = St.add start set in (M.bind (pred start) (fun b -> if b then return (Some [])
                                                                    else (
                                                                      M.bind (f start) (
                                                                        fun succs -> (
                                                                          let rec finish = function 
                                                                          | [] -> M.return None
                                                                          | (x :: xs) -> if (St.mem x set') then (finish xs) else (M.bind (dfsM' f pred x set') (
                                                                                           fun res -> match res with
                                                                                           | None -> finish xs
                                                                                           | Some ys -> M.return (Some (x :: ys))
                                                                                         ))
                                                                  in (finish succs)
                                                                        )
                                                                        )
                                                                      )
                                                                    ))

let rec dfsM : ({O : Ord} -> {M : Monad} -> (O.t -> (O.t list) M.t) -> (O.t -> bool M.t) -> O.t -> ((O.t list) option) M.t) = 
              fun {O : Ord} {M : Monad} (f : O.t -> (O.t list) M.t) (pred : O.t -> bool M.t)  (start : O.t) -> dfsM' {O} {M} {St.Make{O}} f pred start (St.empty {St.Make{O}})


let dfs : ({O : Ord} -> (O.t -> (O.t list)) -> (O.t -> bool) -> O.t -> ((O.t list) option)) = 
                  fun {O : Ord} (f : O.t -> (O.t list)) (pred : O.t -> bool)  (start : O.t) ->
                      let f' = fun x -> Identity.return (f x) in
                      let pred' = fun x -> Identity.return (pred x) in
                      runIdentity (dfsM f' pred' start)