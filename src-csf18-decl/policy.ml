(** Origin separation policy *)

open Level ;;
open Policyfun ;;
open Io ;;

(** [label_input input] assigns a security level to an input event [input] *)
let label_input input =
        match input with 
        | User_load_in_new_win_event(url) -> L
        | User_load_in_win_event(user_window, url) -> L
        | User_link_to_new_win_event(user_window, url) -> L
        | User_link_to_named_win_event(user_window, name, url) -> L
        | User_close_win_event(user_window) -> L
        | User_input_text_event(user_textbox, text, _) -> begin
               match user_textbox with 
               | User_textbox(user_window, nat) ->                              
                     match user_window with 
                     | User_window(url, nat2) -> level_of_url url
                     (* Change to H to suppress the events *)
                end      
        | User_click_button_event(user_button) -> L
        | Network_response_event(net_conn, resp) -> L
        

(** [label_output output] assigns a security level to output event [output] *)
let label_output output = 
        match output with 
        | UI_win_opened_event -> H
        | UI_win_closed_event(user_window) -> H
        | UI_page_loaded_event(user_winndow, url, rendered_doc) -> H
        | UI_page_updated_event(user_window, rendered_doc) -> H
        | UI_alert(text)-> H
        | UI_error(text) -> H
        | Network_send_event (domain, req) -> M(domain)

let label_node node =
        (* node is input event here due to the way FWF is modeled - or write separate functions for different nodes *)
        (* we may want to integrate labels at the low-level of the nodes *)
        match node with
        | User_click_button_event (User_button (User_window (_, 0), n)) -> if n >= 0 then HD else H (* As there are no buttons on page, we label them HD for user level events *)
        | User_input_text_event (User_textbox (User_window (_, 0), n), _, _) -> if n <= 2 then L else HD (* All textboxes are already existing until 2 *)
        | _ -> H (* default label is H *)

(** [pi label input] projection function of input event [input] to a level [label]*)  
let pi label input = 
        if (leq (label_input input) label) then 
                (Event input)
        else 
           match input with
           | User_input_text_event(tb, _, _) -> if label_node input = HD then Suppress else Event input
                (* Does not release text box value on Suppress if label is HD *)
           | User_click_button_event(bt) -> 
                if label_node input = HD then Suppress else Event input
                (* Suppress if the label of the node is HD else release input event to lower levels *)
           | _ ->  Suppress           

(** [ro input] defines a of additional levels for input [input] *)
let ro (input: input_event): Level.level list =  []
