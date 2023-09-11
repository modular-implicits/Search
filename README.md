# search
A collection of generic graph search algorithms

DFS Examples:

Simple DFS returning one path:

```ocaml
let edges n = match n with 
              | 0 -> [0;1;2]
              | 1 -> [3;5;6;7;8;9]
              | 2 -> [3]
              | 3 -> [] 
              | _ -> []

let path = dfs {Int} edges (fun n -> n = 3) 0

```

Using `dfsM` to return all possible paths:

```ocaml
let all_paths = let edges' n = List'.map (fun x -> [x]) (edges n) in
         let pred x = [x = 3] in 
         dfsM  edges' pred 0
```