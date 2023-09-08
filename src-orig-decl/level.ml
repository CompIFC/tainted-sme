(* Defines security levels *)

open Domain ;;

(** The security level *)
type level = H 
            | M of domain
            | L

(* Initial levels *)
let all_levels = 
        [L; H]

