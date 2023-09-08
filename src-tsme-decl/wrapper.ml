(** The definition and implementation of a wrapper. *)

open Label ;;
open Policyfun ;;
open Io;;
open Browser.Impl ;;
open Policy ;;
open Release ;;
       
(** A mapping from security label to a reactive system state *)
type pointer = label -> state

type dec_channel = label -> value list

(** A type of the state of the wrapper *)
type wrapper_state = {
        p: pointer;
        (** Points every label to the state of the original reactive system *)
        ls: label list;   
        (** List of labels on which the wrapper has still to produce output*)
        all_ls : label list; 
        (** All the labels at which the reactive system is running *)   
        dCh : dec_channel;      
}

type waiting_w = wrapper_state

type running_w = wrapper_state

type state_w = 
        | Waiting_w of waiting_w
        | Running_w of running_w 


(**  An initial state of the wrapper. *)        
let start_w = Waiting_w ({p= (fun lev -> start); ls=[]; all_ls=all_levels; dCh= (fun lev -> [])})

  (** {2 Internal wrapper functions} *)

exception WrongStateInUpdate

(** [update_pointer lev p ie] if [p lev] is a waiting state, then updates 
the pointer [p] with the new state upon consuming the input [ie]. Otherwise 
return [p].*)
let update_pointer (lev: label) (p: pointer) (ie:input_event) (dCh:dec_channel)
: pointer = 
match (p lev) with 
   | Running rw -> raise WrongStateInUpdate      
   | Waiting sw ->   
       match (pi lev ie) with 
         | Suppress -> p
         | Event (ie') ->  
              let (s2, oes2) = receive ie' sw (dCh lev) in 
              (fun lev' -> if lev'=lev then s2
                           else p lev')       

let rec dec_update_pointer (lev: (label * input_event) list) (p: pointer) (dCh:dec_channel)
: pointer = 
match lev with 
| [] -> p
| (lev, ie)::tl -> 
    let p' = 
    (match (p lev) with 
    | Running rw -> raise WrongStateInUpdate      
    | Waiting sw -> let (s2, oes2) = receive ie sw (dCh lev) in 
                    (fun lev' -> if lev'=lev then s2 else p lev')) in 
    dec_update_pointer tl p' dCh


let rec update_dec_channel (dv:value) (dCh:dec_channel) (lev:(label * input_event) list)
: dec_channel =
match lev with 
| [] -> (fun lev' -> dCh lev')
| (lev, ie)::tl -> let dCh' = (fun lev' -> if lev'=lev then dv::(dCh lev) else dCh lev') in 
                    update_dec_channel dv dCh' tl   


(** [update_pointers levs p ie] at all the labels in [levs] moves the pointer 
[p] to the producer state corresponding to comsuming input [ie]. At all these 
labels the pointer must be in a waiting state. *)   
let rec update_pointers (levs: label list) (p: pointer) (ie:input_event) (dCh:dec_channel)
: pointer =  
      match levs with 
      [] -> p
      | lev::tl ->  let p' = (update_pointer lev p ie dCh) in                
                    (update_pointers tl p' ie dCh)


let rec upper_acc levs ie acc= 
        match levs with 
        | [] -> acc
        | lev::tl -> 
        begin match (pi lev ie) with 
           | Event input -> 
                 upper_acc tl ie (List.append acc [lev])
           | Suppress -> upper_acc tl ie acc
        end

(** [upper levs ie] defines a list of upper security labels for input event 
[ie] from the list of all labels [levs]. *)
let upper (levs: label list) (ie: input_event) = 
        upper_acc levs ie []

let rec upper_dec (lev:(label * input_event) list) = 
        match lev with 
        | [] -> []
        | (l, ie)::lev' -> l::(upper_dec lev')


exception WrongStateInGetOutput

(** [get_output p ie] produces output of the copy at label [label_input ie] on 
input [ie]. The current state of this copy should be a waiting state. *)
let get_output_sub (p: pointer) (ie:input_event) (lev:label) (dCh:dec_channel)
: (output_event) list = 
    (match (p lev) with 
    Waiting sw ->     
        let (s2, oes2) = receive ie sw (dCh lev) in 
        (oes2)
    | Running rw -> raise WrongStateInGetOutput)

let rec get_output (p: pointer) (ie:input_event) (lev:label list) (dCh:dec_channel)
: (output_event) list = 
    match lev with | [] -> [] | hd::tl -> (get_output_sub p ie hd dCh)@(get_output p ie tl dCh)

let rec get_dec_output (p: pointer) (lie:(label * input_event) list) (dCh:dec_channel)
: output_event list = 
match lie with 
| [] -> []
| (lev, ie)::tl -> let o = begin
    match (p lev) with 
    | Waiting sw -> let (s2, oes2) = receive ie sw (dCh lev) in oes2
    | Running rw -> raise WrongStateInGetOutput   
    end in o@(get_dec_output p tl dCh)
   
(** [filter_outputs_acc os lev new_add_levs acc] is an auxiliary function for 
[filter_output]. *)
let rec filter_outputs_acc os lev new_add_levs acc = 
   match os with 
   | [] -> acc
   | oe::os2 -> let lev_oe = (label_output oe) in
       if (leq lev lev_oe || (List.mem lev_oe new_add_levs)) then 
            (filter_outputs_acc os2 lev new_add_levs (List.append acc [oe]))
       else (filter_outputs_acc os2 lev new_add_levs acc)

(** [filter_outputs os lev new_add_levs] filters list of outputs [os] leaving 
only those that are at label [lev] and higher. Also leaves those output events 
that are at label from the new list of labels [new_add_levs]. *)
let filter_outputs (os: output_event list) (lev : label) (new_add_levs : label list): output_event list = 
        filter_outputs_acc os lev new_add_levs []


(** [find_new_labels all_ls ls new_ls] finds labels in the list of labels [ls] 
that are not in the list [all_ls] and put them in a list [new_ls]. *)
let rec find_new_labels(all_ls: label list) (ls: label list) (new_ls: label list): label list = 
        match ls with 
        | [] -> new_ls 
        | hd::tl -> 
            if (List.mem hd all_ls) then 
                 find_new_labels all_ls tl new_ls             
            else find_new_labels all_ls tl (new_ls @ [hd])

(** [make_new_copies ls lev p] makes copies of a state [p lev] and for every label 
[l] in a list [ls] defines [p l] := [p lev]. *)
let rec make_new_copies (ls: label list) (lev: label) (p: pointer): pointer =
        match ls with 
        | [] -> p
        | hd::tl -> 
            let p = (fun lev' -> if hd=lev' then (p lev)
                                 else (p lev') ) in
            make_new_copies tl lev p            


(** [filter_one_output oe run_lev levs] returns a list containing [oe] if the label 
of [oe] is [run_lev] or if it's higher than [run_lev] and is not in the list of 
currently running labels [levs]. Otherwise returns an empty list. *)
let filter_one_output (oe: output_event) (run_lev: label) (levs: label list)
: output_event list =          
        let lev_oe = (label_output oe) in 
        if (lev_oe = run_lev) then [oe]
        else 
           ( if (leq run_lev lev_oe) then                  
                ( if (List.mem lev_oe levs) 
                        then []
                        else [oe] )        
             else []  )


(** An auxiliary function for a function [filter_outputs_one_label oes run_lev levs]. *)
let rec filter_outputs_one_label_acc (oes: output_event list) (run_lev: label) (levs: label list) (acc:  output_event list): output_event list = 
        match oes with 
        | [] -> acc
        | oe::oes2 ->                       
               filter_outputs_one_label_acc oes2 run_lev levs 
                (List.append acc (filter_one_output oe run_lev levs))

(**
[filter_outputs_one_label oes run_lev levs] filters outputs [oes] leaving only those that 
are at label [run_lev].
*)
let filter_outputs_one_label (oes: output_event list) (run_lev: label) (levs: label list): output_event list = 
        filter_outputs_one_label_acc oes run_lev levs []

let rec get_dec_levels (dv:(label * input_event) list) =
    match dv with 
    | [] -> []
    | (l, ie)::tl -> l::(get_dec_levels tl)

  (** {2 Top-label wrapper functionality} *)

(** [receive_w ie w] respond to an incoming event [ie] while being in a waiting state [w]. 
The result is a new state and an output_event list. *)
let receive_w (ie : input_event) (w: waiting_w) 
: state_w * output_event list = 
        let lev = (label_input_node ie (w.p (H, L))) in (* we check the topmost node as that is the one the user interacts with *)

        (* additional labels for ie *)
        let add_levs = (ro ie) in 

        let (dv, dec_ie_levs) = (match lev with
                                | (M _, M _) -> let r = (module HH : ReleaseModule) in
                                            let module R = (val r) in
                                    (match (R.relfunc ie) with
                                    | (dv, None) ->  (dv, [])
                                    | (dv, Some ie') -> (dv, [(L, H), ie']))
                                | _ -> ("", [])) in    

        (* if lev is also a new label, add it to a list *)
        let new_lev = (if List.mem lev w.all_ls then [] 
                       else [lev]) in        

        let dec_lev = get_dec_levels dec_ie_levs in 

        (* find labels in add_levs that are new among all_ls *)
        let new_add_levs = (find_new_labels w.all_ls (add_levs @ new_lev @ dec_lev) []) in

        let ndCh = update_dec_channel dv w.dCh dec_ie_levs in 

        (* for all new_add_levs make a copy from a lower label *)
        let p' = (make_new_copies new_add_levs (L, H) w.p) in

        (* build a list of labels to run ie: only those label l when 
         * pi_l(ie) <> Suppress, also add new_add_labels to this list *)
        let upper_list = (insert_labels (upper w.all_ls ie) new_add_levs)@dec_lev in

        let all_ls = (insert_labels w.all_ls new_add_levs) in

        let p'' = (update_pointers upper_list p' ie ndCh) in
        let os = (get_output p' ie upper_list ndCh) in

(*         let pd = dec_update_pointer dec_ie_levs p'' ndCh in 
        let osd = (get_dec_output p'' dec_ie_levs ndCh) in 
 *)
        let os'= (filter_outputs (os) lev new_add_levs) in
        
        ((Running_w({p=p''; ls=upper_list; all_ls=all_ls; dCh=ndCh})), os')             


(** [continue_w r] advance the wrapper state with one step from the running state [r]. 
Returns a new state and a list  of output_event. *)
let continue_w (r: running_w)
: state_w * output_event list = 
        match r with    

          (*No labels left, so go to the Waiting state *) 
        | ({p = p; ls = []; all_ls=all_ls; dCh=dCh}) ->                                      
                        (Waiting_w({p = p; ls = []; all_ls=all_ls; dCh=dCh}), [])


        | ({p = p; ls = lev::tl; all_ls=all_ls; dCh=dCh}) ->  

           match (p lev) with 
            | Waiting rsw -> 
               (* We skip the label lev if (r.p lev) is a consumer state *)
               (Running_w({p = p; ls = tl; all_ls=all_ls; dCh=dCh}), [])                            

            | Running rsr -> 
                let (s2, oes2) = (continue rsr lev (dCh lev)) in
                let p' = (fun lev' -> if lev'=lev then s2
                                      else p lev') in                 
                let tl' = ( match s2 with 
                            | Waiting _ -> tl
                            | Running _ -> lev::tl
                           ) in
                let oes' = (filter_outputs_one_label oes2 lev all_ls) in 
                     (Running_w({p=p'; ls=tl'; all_ls=all_ls; dCh=dCh}), oes')
               


                
