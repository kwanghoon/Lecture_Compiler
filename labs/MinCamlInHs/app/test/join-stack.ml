let rec f _ = 123 in
let rec g _ = 456 in
let rec h _ = 789 in

let x = f () in
let y = g () in
print_int ((if h () = 0 then x + 1 else y + 2) + x + y)
(* In the then branch, x is in r0 and y is in the stack, 
   while in the else branch, y is in r0 and x is in the stack. *)
