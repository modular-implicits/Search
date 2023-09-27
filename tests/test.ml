module List' = List;;
open SearchM.Algo;;
open implicit Imp.Control;;
open Imp.Data;;
open Imp.Show;;




let edges n = match n with 
              | 0 -> [0;1;2]
              | 1 -> [3;5;6;7;8;9]
              | 2 -> [3]
              | 3 -> [] 
              | _ -> []

let () = let path = dfs {Int} edges (fun n -> n = 3) 0 in 
            print_endline (show path)


let () = let edges' n = List'.map (fun x -> [x]) (edges n) in
         let pred x = [x = 3] in 
         let z = (dfsM  edges' pred 0) in 
          print_endline (show z)

(* This Just fails completely - no idea what to do *)
(*
let () = let edges' (n : int) : int list list = List'.map (fun x -> [x]) (edges n) in
            let pred x = [x = 3] in 
            let z = (dfsMF {Imp.Data.Int} {Imp.Control.List} {Imp.Control.List}  edges' pred 0) in 
              print_endline (show z)

*)