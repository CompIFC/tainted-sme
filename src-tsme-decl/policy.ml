(** Origin separation policy *)

open Label ;;
open Policyfun ;;
open Io ;;
open Release ;;
open Browser.Impl;;

exception WrongStateInGetOutput

let label_input_node (ie: input_event) (s: state) 
  : label =
    match s with | Running r -> raise WrongStateInGetOutput
    | Waiting w -> 
    begin match ie with
    | User_load_in_new_win_event(u) -> label_of_url u
    | User_load_in_win_event(uw, u) -> label_of_url u
    | User_link_to_new_win_event(uw, u) -> label_of_url u
    | User_link_to_named_win_event(uw, str, u) -> label_of_url u
    | User_close_win_event(uw) -> (L, H)
    | User_input_text_event(User_textbox(uw, box_pos), str, _) ->
        begin match win_from_user_window uw w.waiting_state with
        | None -> (L, H)
        | Some(wr) -> 
            begin match textbox_handlers_in_pos wr box_pos w.waiting_state with
            | None -> (L, H)
            | Some(dr, _) -> dr.node_label 
            end
        end
    | User_click_button_event(User_button(uw, but_pos)) ->
        begin match win_from_user_window uw w.waiting_state with
        | None -> (L, H)
        | Some(wr) -> 
            begin match button_handlers_in_pos wr but_pos w.waiting_state with
            | None -> (L, H)
            | Some(dr, _) -> dr.node_label
            end
        end
    | Network_response_event(net_conn, resp) -> (M(domain_of_net_connection net_conn), M(domain_of_net_connection net_conn))
    end
;;

(** [label_input input] assigns a security label to an input event [input] *)
let label_input input =
        match input with 
        | User_load_in_new_win_event(url) -> (L, H)
        | User_load_in_win_event(user_window, url) -> (L, H)
        | User_link_to_new_win_event(user_window, url) -> label_of_url url
        | User_link_to_named_win_event(user_window, name, url) -> label_of_url url
        | User_close_win_event(user_window) -> (L, H)
        | User_input_text_event(user_textbox, text, _) -> begin
               match user_textbox with 
               | User_textbox(user_window, nat) ->                              
                     match user_window with 
                     | User_window(url, nat2) -> label_of_url url
                end      
        | User_click_button_event(User_button (uw, pos)) -> (L, H) 
                (* if pos = 0 then (H, L) else (L, L) *)
        | Network_response_event(net_conn, resp) ->  (* (L, H)*)
                        (M(domain_of_net_connection net_conn), M(domain_of_net_connection net_conn))
        

(** [label_output output] assigns a security label to output event [output] *)
let label_output output = 
        match output with 
        | UI_win_opened_event -> (H, L)
        | UI_win_closed_event(user_window) -> (H, L)
        | UI_page_loaded_event(user_window, url, rendered_doc) -> (H, L)
        | UI_page_updated_event(user_window, rendered_doc) -> (H, L)
        | UI_alert(text)-> (H, L)
        | UI_error(text) -> (H, L)
        | Network_send_event (domain, req) -> (M(domain), M(domain))

let label_node node = 
        match node with
        | User_click_button_event (User_button (User_window (_, 0), n)) -> if n >= 0 then (H, L) else (H, H) (* As there are no buttons on page, we label them HD for user level events *)
        | User_input_text_event (User_textbox (User_window (_, 0), n), _, _) -> if n <= 2 then (L, H) else (H, L) (* All textboxes are already existing until 2 *)
        | _ -> (L, H) (* default label is L, H *)

(** [pi label input] projection function of input event [input] to a label [label]*)  
let pi label input = 
        if (leq (label_input input) label) then 
                (Event input)
        else Suppress

(** [ro input] defines a list of additional labels for input [input] *)
let ro (input: input_event): label list =  []

(* Declassification Function for HH *)
module HH : ReleaseModule = struct
 let counter = ref 0
 let value = ref 0
 (* 
    write the declassify function here 
    returns releaseval and event option
 *)
 let relfunc input = 
    match input with
           | User_input_text_event(tb, _, _) -> ("button clicked", Some input)
           | User_click_button_event(bt) -> ("key pressed", Some input)
           | _ ->  ("", None)
end 
;;
