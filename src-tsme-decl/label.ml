(* Defines security labels *)

open Domain ;;

(** The security level *)
type level = H 
            | M of domain
            | L

(* Confidentiality & Integrity levels *)
type label = (level * level)

(* Initial levels *)
let all_levels = 
        [(L, H); (L, L); (H, H); (H, L)]

let getILabel (l:label) =
    let (c,i) = l in i

let getCLabel (l:label) =
    let (c,i) = l in c

let cLabelLessThanEq lab1 lab2 =
    (lab1 = lab2) ||
    (match lab1, lab2 with
    | L, _ -> true
    | _, H -> true
    | _, _ -> false)

let iLabelLessThanEq lab1 lab2 =
    (lab1 = lab2) ||
    (match lab1, lab2 with
    | H, _ -> true
    | _, L -> true
    | _, _ -> false)

let leq (lab1:label) (lab2:label) = 
    iLabelLessThanEq (getILabel lab1) (getILabel lab2) && 
    cLabelLessThanEq (getCLabel lab1) (getCLabel lab2)


