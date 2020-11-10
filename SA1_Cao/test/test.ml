open OUnit2
open Rec.Interp
open Rec.Ds

(* A few test cases *)
let tests_let = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "let x = 0 in let x = 22 in x"));
]


let tests_proc = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 3))
                 (interp "(proc (x) { x+1 } 2)"))
]

let tests_rec = [

    "letrec_even_odd" >:: (fun _ -> assert_equal
                            (* ~printer:string_of_expval *)
                            (Ok (NumVal 1))
                            (interp
                               "letrec
                                  even(x) = if zero?(x) then 1 else (odd (x-1))
                                  odd(x) = if zero?(x) then 0 else (even (x-1))
                                in (odd 99)"));
  "letrec_factorial" >:: (fun _ -> assert_equal
                             (* ~printer:string_of_expval *)
                             (Ok (NumVal 120))
                             (interp
                               "letrec
                                  fac(x) = if zero?(x) then 1 else x*(fac (x-1))
                                in (fac 5)"));


  "letrec_fib" >:: (fun _ -> assert_equal
                             (* ~printer:string_of_expval *)
                             (Ok (NumVal 55))
                             (interp
                               "letrec
  fib2(n) =  (fib (n-2)) 
  fib1(n) =  (fib (n-1))
  fib(n) = 
  if zero?(n)
  then 0
  else (if zero?(n-1)
  then 1
    else 
       (fib1 n) + (fib2 n))
in (fib 10)"))

 
   
]

let _ = run_test_tt_main ("suite" >::: (tests_let @ tests_proc @ tests_rec))
