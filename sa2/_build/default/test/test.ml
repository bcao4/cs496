open OUnit2
open Sool.Ds
open Sool.Interp

(* a unit test has a (name, code, result, points) *)
type unit_test =
  | Interp of string * string * exp_val result * int
(*  | Typchk of string * string * texp tea_result * int *)

let generic_err : 'a result = Error ("<message>")

(* string functions needed for pretty printing *)
let string_of_expval_result : exp_val result -> string = function
  | Ok v -> "Ok (" ^ (string_of_expval v) ^ ")"
  | Error s -> "Error (" ^ s ^ ")"

(*
let string_of_texpr_tea_result : texpr tea_result -> string = function
  | Ok v -> "Ok (" ^ (string_of_texpr v) ^ ")"
  | Error s -> "Error (" ^ s ^ ")"
*)

(* turn our idea of a unit test into an oUnit test *)
let test_of_unit_test (test : unit_test) : OUnit2.test =
  let fail_message name points =
    name ^ " failed, -" ^ (string_of_int points) ^ " point(s)"
  in
  let enhanced_assert name points result target =
    let fail = fail_message name points in
    assert_equal
    ~msg:(fail)
    ~printer:string_of_expval_result
    result target
  in
  let err_test name code points func =
    name >:: (fun _ ->
      let target =
        try func code
        with e -> Error (Printexc.to_string e)
      in
      match target with
      | Error _ -> ()
      | Ok _ as value -> enhanced_assert name points generic_err value)
  in
  let normal_test name code result points func =
    name >:: (fun _ ->
      let target =
        try func code
        with e -> Error (Printexc.to_string e)
      in
      enhanced_assert name points result target)
  in
  match test with
  | Interp (name, code, Error _, points) ->
      err_test name code points interp
  | Interp (name, code, result, points) ->
      normal_test name code result points interp
  (*
  | TypChk (name, code, Error _, points) ->
      err_test name code points chk
  | TypChk (name, code, Ok _ as result, points) ->
      normal_test name code result points chk
  *)

let make_suite (tests : unit_test list list) : OUnit2.test list =
  List.(map test_of_unit_test (concat tests))


(* ----------------------------------------------------
 * Define the test suite below and assign point values.
 * ---------------------------------------------------- *)

(* NOTE: the tests shown below are just examples for demonstrating how errors
 * will be reported, and to show how to construct tests. *)

let arith : unit_test list = [
  Interp ("positive-const", "11", Ok (NumVal 11), 0);
  Interp ("negative-const", "(-11)", Ok (NumVal (-11)), 0);
  Interp ("simple-arith-1", "44-33", Ok (NumVal 11), 0);
  Interp ("nested-arith-left", "(44-33)-22", Ok (NumVal (-11)), 0);
  Interp ("nested-arith-right", "55-(22-11)", Ok (NumVal (44)), 0);
(*  TypChk ("const-type", "11", Ok (IntType), 1); *)
]

let implicit_refs : unit_test list = [
    Interp("implicit-refs","let g =
      let counter = 0
      in proc(d) {
         begin
           set counter = counter+1;
           counter
         end
         }
   in (g 11)-(g 22)",Ok (NumVal (-1)),1);
  Interp("implicit-refs2", "
let a = 3
in let f = proc(x) { proc(y) { set x = x-y }}
in begin
    ((f a) 2);
    a
   end",Ok (NumVal 3),1)
  ]

let vars : unit_test list = [
  Interp ("unbound-variable-1", "foo", generic_err, 0);
  Interp ("unbound-variable-2", "1+(2-foo)", generic_err, 0);
]

let create_object = "
class c extends object {
  method init() { 0 }
}
let o = new c() in 3
"

let classes : unit_test list = [
  Interp ("create-empty-class",
          "class c extends object {} 3",
          Ok (NumVal 3),
          0);
  Interp ("create-class-with-field-and-method", "
          class c extends object {
            field y
            method gety() { y }
          } 33",
          Ok (NumVal 33),
          0);
  Interp ("create-object", create_object, Ok (NumVal 3), 0);

  Interp("simple1","
class c1 extends object { 
  field s 
  method initialize() {set s = 44}
  method gets() {s}
  method sets(v) {set s = v}
  }
let o1 = new c1() in send o1 gets()",  Ok (NumVal 44),0);



  Interp("field_and_param_scope1","
class c1 extends object {
 field x
 field y
 method initialize () {
  begin
   set x = 11;
   set y = 12
end }
method m1 () { x +y }
method m2 () {  send self m3() }
}

class c2 extends c1 {
 field y
 method initialize () {
  begin
   super initialize();
   set y = 22
end }
method m1 (u,v) {  x + y -v }
method m3 () { 7}
}

class c3 extends c2 {
 field x
 field z
 method initialize () {
  begin
   super initialize();
   set x = 31;
   set z = 32
end
}
method m3 () {  x + y + z}
}

let o3 = new c3()
in send o3 m1(7,8)
",Ok (NumVal 25),0);


  Interp("send-msg1","
class c1 extends object  {
  field s 
  method initialize() {set s = 44}
  method gets() {s}
  method sets(v) {set s = v}
 } 
let o1 = new c1() 
in let    t1 = 0
in let    t2 = 0 
in begin
     set t1 = send o1 gets();
     send o1 sets(33);
     set t2 = send o1 gets();
     list(t1, t2)
  end",Ok (ListVal [NumVal 44; NumVal 33]),0);

 Interp("self1","class c extends object  {
  field s
  method initialize(v) {set s = v}
  method sets(v) {set s = v}
  method gets() { s}
  method testit() {send self sets(13) }
  }
let o = new c (11)
in let       t1 = 0
in let       t2 = 0
   in begin 
       set t1 = send o gets();
       send o testit();
       set t2 = send o gets();
       list(t1,t2)
      end",Ok (ListVal [NumVal 11; NumVal 13])
       ,0);

  Interp("counter-1","
class counter extends object  {
 field count
 method initialize(){set count = 0}
 method countup() {set count = count + 1}
 method getcount(){count}
 }
let o1 = new counter ()
 in let   t1 = 0
in let    t2 = 0
in begin
    set t1 = send o1 getcount();
    send o1 countup();
    set t2 = send o1 getcount();
    list(t1,t2)
   end",Ok (ListVal [NumVal 0; NumVal 1]),0);

  Interp("sharedf-counter","

class counter extends object {
  field count
   method initialize() {set count = 0}
   method countup() {set count = count+1}
   method getcount(){ count}
   }
class c1 extends object  {
   field n
   field counter1
   method initialize(a_counter) {
    begin
     set n = 0;
     set counter1 = a_counter
    end}
   method countup(){
     begin
      send counter1 countup();
      set n = n+1
     end}
   method getstate(){list(n, send counter1 getcount())}
 }  
let counter1 = new counter()
in let o1 = new c1(counter1)
 in let      o2 = new c1(counter1)
in begin
     send o1 countup();
     send o2 countup();
     send o2 countup();
     list( send o1 getstate(),
           send o2 getstate())
   end",Ok (ListVal [ListVal [NumVal 1; NumVal 3]; ListVal [NumVal 2; NumVal 3]]),0);


]

(* add lists of tests to the suite here *)
let _ = run_test_tt_main ("suite" >::: (make_suite [
  arith;
  vars;
  implicit_refs;
  classes;
]))
