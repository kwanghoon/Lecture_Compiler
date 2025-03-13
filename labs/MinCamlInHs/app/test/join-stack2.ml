let rec f _ = 123 in
let rec g _ = 456 in

let x = f () in
print_int ((if x <= 0 then g () + x else x) + x)
(* x is saved in the then branch but not in the else branch. )
( Furthermore, x is in r0 in the then branch and in r1 in the else branch. *)
