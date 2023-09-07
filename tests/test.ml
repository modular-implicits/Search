open Imp.Show
open Generics.Generic
open Generics.Memo

type basicSum = L of int | R of string

implicit module GenBasicSum = struct 
  type t = basicSum 
  type rep = (int genBasic, string genBasic) genSum
  let toRep = function 
                  | L x -> Left (GenBasic ("L", x))
                  | R x -> Right (GenBasic ("R", x))
  let fromRep = function
                | Left (GenBasic (_, x)) -> L x
                | Right (GenBasic (_, x)) -> R x
end

type basicProd = P of int * string

let () = 
begin
  let open [@warning "-33"] Generics.GenShow in
  assert (show (L 1) = "L 1");
end

(* Testing Memo *)

type ints = L1 of int | R1 of int
  
implicit module IntsGeneric = struct
  type t = ints
  type rep = (int genBasic, int genBasic) genSum
  let (toRep : ints -> (int genBasic, int genBasic) genSum) = function
    | L1 a -> Left (GenBasic ("L1", a))
    | R1 b -> Right (GenBasic ("R1", b))
  let (fromRep : (int genBasic, int genBasic) genSum -> ints) = function
    | Left (GenBasic (_, a)) -> L1 a
    | Right (GenBasic (_,b)) -> R1 b
end

let rec fib : int -> int = function 
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2)

let weirdFib : ints -> int = function 
  | (L1 i) -> i
  | (R1 i) -> fib i

let () =
begin
  let m = memo weirdFib in
  print (m (R1 46));
  print (m (R1 46));
  print (m (R1 46));
end
