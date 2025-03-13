let rec f _ = 123 in
let rec g _ = 456 in
let rec h _ = 789 in

let x = f () in
print_int ((if x <= 0 then g () + x else h () - x) + x)
(* x is in r1 in both the then and else branches. )
( However, before the if statement, x is in r0. *)
