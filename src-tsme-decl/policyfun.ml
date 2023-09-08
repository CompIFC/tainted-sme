(** All functions for assigning security labels to inputs and outputs. *)

open Domain ;;
open Io ;;
open Label ;;

(** A type of input event with a special [Suppress] event *)
type input_event_pi = 
        | Event of input_event
        | Suppress

(** [domain_of_url u] defines a domain of url [u]*)        
let domain_of_url u = 
        match u with 
        | Blank_url -> None           
        | Http_url (domain, ru) -> Some(domain)


(** [ label_of_url u] defines a label of url as M(domain)*)        
let label_of_url u = 
        match u with 
        | Blank_url -> (L, H)            
        | Http_url (domain, ru) -> (M(domain), M(domain))

(** [insert_new_label_acc lev ls acc] is an auxiliary function to insert a new label [lev]  into a list of labels [ls], [acc] is an accumulative list assuming that [lev] is not in the list [ls]. *)        
let rec insert_new_label_acc (lev: label)  (ls: label list) (acc: label list) =      
        match ls with 
        | [] -> acc
        | lev'::tl -> 
            (* We already know that lev is not in the ls list *)                        
             if (leq lev' lev) then 
                     insert_new_label_acc lev tl (List.append acc [lev'])                               
             else (List.append acc (lev::ls))

(** [insert_new_label lev ls ] inserts a new label [lev]  into a list of labels [ls] using [insert_new_label_acc]*)
let insert_new_label (lev: label)  (ls: label list) = insert_new_label_acc lev ls []

(** [insert_labels all_ls ls] inserts list of labels [ls] into a list [all_ls] assuming that [all_ls] does not contain any label from list [ls]. *)
let rec insert_labels (all_ls: label list) (ls: label list) =
        match ls with 
        | [] -> all_ls
        | lev::tl -> let all_ls' = (insert_new_label lev all_ls) in
                    insert_labels all_ls' tl    

(** [labels_from_domains_acc domains labels] is an auxiliary function that builds a list of labels [labels] from domains [domains]*)
let rec labels_from_domains_acc domains labels = 
        match domains with
        | [] -> labels
        | hd::tl -> labels_from_domains_acc tl (labels @ [(M(hd), M(hd))])

(** [labels_from_domains domains] builds a list of labels from domains [domains] using  [labels_from_domains_acc] *)
let labels_from_domains domains = labels_from_domains_acc domains []        

(** {2 Projections functions} *)

(** [project_doclist dom doclist doclist'] builds a projection of the [doc] list [doclist] on the domain [dom]. the result is [doc] list [doclist']. *) 
let rec project_doclist dom doclist doclist' =        
match doclist with 
        | [] -> doclist' 
        | hd::tl -> begin
          match hd with 
          | Para(_,_)
          | Link(_, _,_) 
          | Textbox(_,_)
          | Button(_, _) 
          | Inl_script(_, _) -> 
                project_doclist dom tl doclist'          

          | Rem_script(_, url) -> 
                let dom_optional = (domain_of_url url) in                          
                begin
                match dom_optional with 
                | None -> project_doclist dom tl doclist'
                | Some(dom2) -> 
                     if (dom2=dom) then project_doclist dom tl (doclist' @ [hd])
                     else project_doclist dom tl doclist'
                end
          | Div(_, dlist) -> project_doclist dom (tl @ dlist) doclist'
        end

(** [proj dom body] builds a projection of the body of the receive event [body] to the domain [dom] *)  
let proj dom body = 
        match body with 
        | Empty_file 
        | Script_file (_) -> 
                        body
        | Html_file doclist -> 
                let doclist' = (project_doclist dom doclist []) in
                Html_file doclist'                

(** [add_domains doclist domains] adds a list of domains of the remote script urls from the list of docs [doclist] into the list of domains [domains]. *)
let rec add_domains doclist domains= 
        match doclist with 
        | [] -> domains 
        | hd::tl -> begin
          match hd with 
          | Para(_,_)
          | Link(_, _,_) 
          | Textbox(_,_)
          | Button(_, _)
          | Inl_script(_, _) -> 
                  add_domains tl domains
          | Rem_script(_, url) -> 
                  let dom_optional = (domain_of_url url) in 
                  begin
                  match dom_optional with 
                  | None -> add_domains tl domains
                  | Some(dom) -> add_domains tl (List.append domains [dom])
                  end
          | Div(_, dlist) -> add_domains (List.append tl dlist) domains
        end

(** [build_new_domains body] in case [body] is an Html_file, builds a list of domains of the remote scripts in the [body] using function [add_domains]. Otherwise return an empty list. *)
let build_new_domains body =         
        match body with 
        | Empty_file -> []
        | Script_file expr -> []
        | Html_file doclist -> add_domains doclist []     

(** [domain_of_net_connection nc] returns a domain of a Net_connection [nc]. *)        
let domain_of_net_connection nc = 
        match nc with                         
            | Net_connection (domain, i) -> domain         

  (** {2 Printing and to_string operations} *)                       

(** [string_of_string_list ls acc] transforms a string list [ls] to a string [acc] in a reversed order, e.g. "a", "b", "c" to "cba" *)
let rec string_of_string_list ls acc = 
        match ls with 
        | [] -> acc
        | hd::tl -> string_of_string_list tl acc ^ "." ^ hd

(** [string_of_domain d] transforms a domain [d] into a string *)
let string_of_domain d = string_of_string_list d.domain_value ""

(** [string_of_label l] transforms a security label [l] to a string *)
let string_of_label l = 
        match l with 
        | H -> "high" 
        | M (domain) -> "medium: " ^ (string_of_domain domain)
        | L -> "low" 
            
(** [string_of_url url] transforms a url [url] into a string, if [url] is empty, transforms to "blank". *)                     
let string_of_url url =                   
      match url with 
        | Blank_url -> "blank"            
        | Http_url (domain, ru) -> (string_of_domain domain)


(**  [string_of_output output] transforms an output [output] to a string *)
let string_of_output output = 
        match output with 
        | UI_win_opened_event -> "UI_win_opened_event"
        | UI_win_closed_event(user_window) -> 
              "UI_win_closed_event"
        | UI_page_loaded_event(user_winndow, url, rendered_doc) ->
              "UI_page_loaded_event with " ^ (string_of_url url)
        | UI_page_updated_event(user_window, rendered_doc) ->
              "UI_page_updated_event"
        | UI_alert(text)-> "UI_alert"
        | UI_error(text) -> "UI_error"
        | Network_send_event (domain, req) -> 
               "Network_send_event with " ^ (string_of_domain domain) 

(** [print_outputs oes] recursively prints a list of outputs [oes] *)
let rec print_outputs oes = match oes with 
        | [] -> print_string("\n")
        | hd::tl -> print_endline(string_of_output hd ^ "; "); print_outputs tl            
(** [print_labels ls] recursively prints a list of labels [ls] *)
let rec print_labels ls = 
        match ls with 
        | [] -> print_string("\n")
        | (lc, li)::tl -> (print_string(string_of_label(lc) ^ " " ^ string_of_label(li) ^ "; "); 
        (print_labels tl))
        

