(** The definition and implementation of a wrapper. *)

open Level ;;
open Policyfun ;;
open Browser.Impl ;;
open Policy ;;
       
(** A mapping from security level to a reactive system state *)
type pointer = level -> state

(** A type of the state of the wrapper *)
type wrapper_state = {
        p: pointer;
        (** Points every level to the state of the original reactive system *)
        ls: level list;   
        (** List of levels on which the wrapper has still to produce output*)
        all_ls : level list; 
        (** All the levels at which the reactive system is running *)         
}

type waiting_w = wrapper_state

type running_w = wrapper_state

type state_w = 
        | Waiting_w of waiting_w
        | Running_w of running_w 


(**  An initial state of the wrapper. *)        
let start_w = Waiting_w ({p= (fun lev -> start); ls=[]; all_ls=all_levels})

  (** {2 Internal wrapper functions} *)

exception WrongStateInUpdate

(** [update_pointer lev p ie] if [p lev] is a waiting state, then updates 
the pointer [p] with the new state upon consuming the input [ie]. Otherwise 
return [p].*)
let update_pointer (lev: level) (p: pointer) (ie:input_event) 
: pointer = 
match (p lev) with 
   | Running rw -> raise WrongStateInUpdate      
   | Waiting sw ->   
       match (pi lev ie) with 
         | Suppress -> p
         | Event (ie') ->  
              let (s2, oes2) = receive ie' sw in 
              (fun lev' -> if lev'=lev then s2
                           else p lev')       


(** [update_pointers levs p ie] at all the levels in [levs] moves the pointer 
[p] to the producer state corresponding to comsuming input [ie]. At all these 
levels the pointer must be in a waiting state. *)   
let rec update_pointers (levs: level list) (p: pointer) (ie:input_event)
: pointer =  
      match levs with 
      [] -> p
      | lev::tl ->  let p' = (update_pointer lev p ie) in                
                    (update_pointers tl p' ie)


let rec upper_acc levs ie acc= 
        match levs with 
        | [] -> acc
        | lev::tl -> 
        begin match (pi lev ie) with 
           | Event input -> 
                 upper_acc tl ie (List.append acc [lev])
           | Suppress -> upper_acc tl ie acc
        end

(** [upper levs ie] defines a list of upper security levels for input event 
[ie] from the list of all levels [levs]. *)
let upper (levs: level list) (ie: input_event) = 
       upper_acc levs ie []


exception WrongStateInGetOutput

(** [get_output p ie] produces output of the copy at level [label_input ie] on 
input [ie]. The current state of this copy should be a waiting state. *)
let get_output (p: pointer) (ie:input_event) 
: output_event list = 
    (match (p (label_input ie)) with 
        Waiting sw ->     
            let (s2, oes2) = receive ie sw in  oes2
        | Running rw -> raise WrongStateInGetOutput)

   
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
only those that are at level [lev] and higher. Also leaves those output events 
that are at level from the new list of levels [new_add_levs]. *)
let filter_outputs (os: output_event list) (lev : level) (new_add_levs : level list): output_event list = 
        filter_outputs_acc os lev new_add_levs []


(** [find_new_levels all_ls ls new_ls] finds levels in the list of levels [ls] 
that are not in the list [all_ls] and put them in a list [new_ls]. *)
let rec find_new_levels(all_ls: level list) (ls: level list) (new_ls: level list): level list = 
        match ls with 
        | [] -> new_ls 
        | hd::tl -> 
            if (List.mem hd all_ls) then 
                 find_new_levels all_ls tl new_ls             
            else find_new_levels all_ls tl (new_ls @ [hd])

(** [make_new_copies ls lev p] makes copies of a state [p lev] and for every level 
[l] in a list [ls] defines [p l] := [p lev]. *)
let rec make_new_copies (ls: level list) (lev: level) (p: pointer): pointer =
        match ls with 
        | [] -> p
        | hd::tl -> 
            let p = (fun lev' -> if hd=lev' then (p lev)
                                 else (p lev') ) in
            make_new_copies tl lev p            


(** [filter_one_output oe run_lev levs] returns a list containing [oe] if the level 
of [oe] is [run_lev] or if it's higher than [run_lev] and is not in the list of 
currently running levels [levs]. Otherwise returns an empty list. *)
let filter_one_output (oe: output_event) (run_lev: level) (levs: level list)
: output_event list =          
        let lev_oe = (label_output oe) in 
        if (lev_oe = run_lev) then [oe]
        else 
           ( if (leq run_lev lev_oe) then                  
                ( if (List.mem lev_oe levs) 
                        then []
                        else [oe] )        
             else []  )


(** An auxiliary function for a function [filter_outputs_one_level oes run_lev levs]. *)
let rec filter_outputs_one_level_acc (oes: output_event list) (run_lev: level) (levs: level list) (acc:  output_event list): output_event list = 
        match oes with 
        | [] -> acc
        | oe::oes2 ->                       
               filter_outputs_one_level_acc oes2 run_lev levs 
                (List.append acc (filter_one_output oe run_lev levs))

(**
[filter_outputs_one_level oes run_lev levs] filters outputs [oes] leaving only those that 
are at level [run_lev].
*)
let filter_outputs_one_level (oes: output_event list) (run_lev: level) (levs: level list): output_event list = 
        filter_outputs_one_level_acc oes run_lev levs []

  (** {2 Top-level wrapper functionality} *)

let rec print_state (l:level list) (p:pointer) =
    match l with 
    | [] -> ()
    | hd::tl -> (match p hd with 
                | Running r -> print_levels([hd]); print_endline("running state")
                | Waiting w -> print_levels([hd]); print_endline("waiting state") );
                (print_state tl p)

(** [receive_w ie w] respond to an incoming event [ie] while being in a waiting state [w]. 
The result is a new state and an output_event list. *)
let receive_w (ie : input_event) (w: waiting_w) 
: state_w * output_event list = 
        let lev = (label_input ie) in      

        (* additional levels for ie *)
        let add_levs = (ro ie) in
        
        (* if lev is also a new level, add it to a list *)
        let new_lev = (if List.mem lev w.all_ls then [] 
                       else [lev]) in        

        (* find levels in add_levs that are new among all_ls *)
        let new_add_levs = (find_new_levels w.all_ls (add_levs @ new_lev) []) in

        (* for all new_add_levs make a copy from a lower level *)
        let p' = (make_new_copies new_add_levs L w.p) in
        
        (* build a list of levels to run ie: only those level l when 
         * pi_l(ie) <> Suppress, also add new_add_levels to this list *)
        let upper_list = (insert_levels (upper w.all_ls ie) new_add_levs) in
        
        let all_ls = (insert_levels w.all_ls new_add_levs) in

        let p'' = (update_pointers upper_list p' ie) in
        let os = (get_output p' ie) in

        let os'= (filter_outputs os lev new_add_levs) in
                ((Running_w({p=p''; ls=upper_list; all_ls=all_ls})), os')             

(** [continue_w r] advance the wrapper state with one step from the running state [r]. 
Returns a new state and a list  of output_event. *)
let continue_w (r: running_w)
: state_w * output_event list = 
        match r with    

          (*No levels left, so go to the Waiting state *) 
        | ({p = p; ls = []; all_ls=all_ls}) ->                                      
                        (Waiting_w({p = p; ls = []; all_ls=all_ls}), [])

          (*Now we run at level lev*) 
        | ({p = p; ls = lev::tl; all_ls=all_ls}) ->   
            
           match (p lev) with 
           | Waiting rsw ->
               (* We skip the level lev if (r.p lev) is a consumer state *)
               (Running_w({p = p; ls = tl; all_ls=all_ls}), [])                            

            | Running rsr ->                 
                let (s2, oes2) = (continue rsr) in
                let p' = (fun lev' -> if lev'=lev then s2
                                      else p lev') in                 
                let tl' = ( match s2 with 
                            | Waiting _ -> tl
                            | Running _ -> lev::tl
                           ) in
                (* print_levels([lev]); print_outputs(oes2); *)
                let oes' = (filter_outputs_one_level oes2 lev all_ls) in 
                     (Running_w({p=p'; ls=tl'; all_ls=all_ls}), oes')
               


                
