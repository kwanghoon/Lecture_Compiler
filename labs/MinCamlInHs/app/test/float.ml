(* When running this test, before calling Main.file, 
  you need to modify Typing.extenv using :=, etc., 
  and explicitly specify the types of external functions 
  such as sin and cos in advance. 
  (Otherwise, MinCaml will automatically infer them as int -> int.) *)
print_int
  (int_of_float
     ((sin (cos (sqrt (abs_float (-12.3))))
         +. 4.5 -. 6.7 *. 8.9 /. 1.23456789)
        *. float_of_int 1000000))
