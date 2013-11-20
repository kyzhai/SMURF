open Printf

(* If you doing want to see the annoy debug information, 
 * simply set debug to be false, the world will be peace
 *)
let debug = true

let trace s = function 
    a -> if debug then 
            ignore(printf "*** %s\n" s)
         else (); (a)

