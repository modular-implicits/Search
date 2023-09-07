module List' = List;;
open Search.Algo;;
open Imp.Control;;
open Imp.Data;;
open Imp.Show;;
open Imp.Control.Foldable;;



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
