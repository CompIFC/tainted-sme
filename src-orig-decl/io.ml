(** Definitions of the types of data exchanged between browsers and the
    internet. *)

open Domain ;;
open Level ;;

(** {2 URLs} *)

(** The type of a directory path, which may end with a directory name or a
    file name. *)
type path = {
  path_value: string list;
  (** A sequence of path elements:
      for example, [\["music"; "artists"; "Bauhaus.html"\]]. *)
}

(** The type of a "request URI" for an HTTP request. *)
type req_uri = {
  req_uri_path: path;
  req_uri_params: string;
  (** The part of the URL after a [?]:
      for example, ["artist=bauhaus&song=slice+of+life"]. *)
}

(** The type of a URL. *)
type url =
  | Blank_url
    (** The URL of a blank browser window (typically written as
        [about:blank]). *)
  | Http_url of domain * req_uri
    (** A URL that begins with [http://]. *)

(** {2 Scripts} *)

(** A representation of the data types in the scripting language. *)
type typ =
  | Null_type
  | Bool_type
  | Int_type
  | String_type
  | Url_type
  | Type_type
  | Function_type
  | Code_type
  | Window_type
  | Node_type

(** A type for variables in the scripting environment. *)
type var = {
  var_name: string;
}

(** An empty type. *)
type void = Void of void

(** A type for script expressions.  These contruct will generate a runtime error
    if they are applied to value of types other than those described in the
    documentation here. *)
type 'a expr =
  | X of 'a
    (** Allows this data type to be extended with additional language constructs
        to be added, which will be needed for the internal language. *)

  | Null             (** The unique value "null". *)
  | Bool of bool     (** A Boolean value. *)
  | Int of int       (** An integer value. *)
  | String of string (** A string value. *)
  | Url of url       (** A URL value. *)
  | Type of typ      (** A value representing a type. *)

  | Seq of 'a expr * 'a expr
    (** Execute two expressions in sequence, discarding the results of the first
        and returning the results of the second. *)
  | If of 'a expr * 'a expr * 'a expr
    (** A conditional expression. *)
  | While of 'a expr * 'a expr
    (** A while loop. *)

  | Prim1 of string * 'a expr
    (** Applies a unary primitive operation to an argument. *)
  | Prim2 of string * 'a expr * 'a expr
    (** Applies a binary primitive operation to two arguments. *)
  | Alert of 'a expr
    (** Sends a message to the user. *)

  | Function of var * var list * 'a expr
    (** [Function(param, locals, body)] is a single-argument function, where
        [locals] are the additional variable names that will be used in the
        body. *)
  | Apply of 'a expr * 'a expr
    (** Applies a function to an argument. *)

  | Code of void expr
    (** A suspended script expression. *)
  | Eval of 'a expr
    (** Forces the execution of a [Code] value. *)

  | Var of var
    (** Looks up a variable in the nearest enclosing scope. *)
  | Set_var of var * 'a expr
    (** Updates a variable in the nearest enclosing scope, or creates a name
        in the statically enclosing window environment if the name is not
        found.  It evaluates to the value of the expression. *)

  | Get_cookie of 'a expr * 'a expr
    (** [Get_cookie(url, key)] gets the cookie value associated with [url].
        Returns null if there is no such cookie. *)
  | Set_cookie of 'a expr * 'a expr * 'a expr
    (** [Set_cookie(url, key, str)] updates (or creates) the cookie value
        associated with [url].  If [str] is [null], then the cookie is
        deleted. *)

  | Xhr of 'a expr * 'a expr * 'a expr
    (** [Xhr(url, msg, handler)] sends an HTTP request to [url] with the
        request body [msg].  If the response is a script and [handler] is a
        function, then the reponse will be supplied as a [Code] value to the
        handler when the response is received.  It is an error if [handler] is
        not a function.  If the response is not a script, the handler will be
        invoked with a [null] argument. *)

  | Self_win
    (** A reference to the statically enclosing window. *)
  | Named_win of 'a expr
    (** [Named_win(str)] evaluates to the window whose string name is [str].
        It evalutes to [null] if there is no such window. *)

  | Open_win of 'a expr
    (** [Open_win(url)] causes [url] to be loaded in a new, unnamed window.  A
        reference to the window is returned.  (The document will not be loaded
        in the window while the current script is running.)  If [url] is [null],
        a new blank window will be opened.  Returns a reference to the opened
        window. *)
  | Open_named_win of 'a expr * 'a expr
    (** [Open_named_win(url, str)] causes [url] to be loaded in the window
        with the name [str].  If a window with name [str] does not exist, then
        it is created.  A reference to the window is returned.  (The document
        will not be loaded in the window while the current script is running.)
        If [url] is null and a window with that name exists, the window will not
        be navigated.  Returns a reference to the window. *)
  | Close_win of 'a expr
    (** [Close_win(win)] closes [win].  It is a no-op if [win] is not open. *)
  | Navigate_win of 'a expr * 'a expr
    (** [Navigate_win(win, url)] causes [url] to be loaded in [win] and
        evaluates to [null].  ([win] will not be ready while the current script
        is running.)  It is an error if [win] is not open. *)

  | Is_win_closed of 'a expr
    (** [Is_win_closed(win)] evaluates to false if [win] refers to an open
      * window and true otherwise. *)
  | Get_win_opener of 'a expr
    (** [Get_win_opener(win)] evaluates to the "opener" of [win] or null if
        [win] has no opener. *)
  | Get_win_location of 'a expr
    (** [Get_win_location(win)] evaluates to the URL that is loaded into the
        window [win].  It is an error if [win] is not open. *)

  | Get_win_name of 'a expr
    (** [Get_win_name(win)] returns the string name of the window, or null if
        the window has no name.  It is an error if [win] is not open. *)
  | Set_win_name of 'a expr * 'a expr
    (** [Set_win_name(win, str)] set the name of [win] to [str], or removes the
        name if [str] is [null].  It evaluates to [null].  It is an error if
        [win] is not open. *)

  | Get_win_root_node of 'a expr
    (** [Get_win_root_node(win)] gets the document root node of a window.  Returns
        null if the window has no document.  It is an error if [win] is not
        open. *)
  | Set_win_root_node of 'a expr * 'a expr
    (** [Set_win_root_node(win, node)] updates the document root node of a
        window.  It evaluates to [null].  It is an error if [win] is not
        open. *)

  | Get_win_var of 'a expr * var
    (** [Get_win_var(win, var)] looks up a variable in a window's environment.  It
        is an error if [win] is not open. *)
  | Set_win_var of 'a expr * var * 'a expr
    (** [Set_win_var(win, var, expr)] updates (or creates) a variable in a
        window's environment and evaluates to the new value.  It is an error if
        [win] is not open and ready. *)

  | New_node of 'a expr
    (** [New_node(desc)] returns a new node of the type matching [desc], which
        should be one of the following: "para", "link", "textbox", "button",
        "inl_script", "rem_script", or "div".  It is an error if [desc] does not
        match one of these strings. *)
  | Get_node_type of 'a expr
    (** [Get_node_type(node)] returns one of the strings "para", "link",
        "textbox", "button", "inl_script", "rem_script", or "div". *)

  | Get_node_contents of 'a expr
    (** [Get_node_contents(node)] returns the contents of nodes that may have
        non-empty contents.  For "para", "link", and "button" nodes, this is a
        [String] value.  For "inl_script" nodes, this is a [Code] value.  It is
        an error if [node] is not one of these types. *)
  | Set_node_contents of 'a expr * 'a expr
    (** [Set_node_contents(node, value)] updates the contents of nodes that may
        have non-empty contents.  For "para", "link", and "button" nodes,
        [value] should be a [String] value.  For "inl_script" nodes, [value]
        should be a [Code] value.  It is an error if [node] is not one of these
        types. *)
  | Get_node_attr of 'a expr * 'a expr
    (** [Get_node_attr(node, attr)] returns the attributes described by the
        string [attr] of the node [node].  All node types have an attribute
        "id", which can be a [Null] or [String] value.  "link" nodes have an
        "href" attribute, which is a [Url] value.  "textbox" nodes have a
        "value" attribute, which is a [String] value.  "rem_script" nodes have a
        "src" attribute, which is a [Url] value. *)
  | Set_node_attr of 'a expr * 'a expr * 'a expr
    (** [Set_node_attr(node, attr, value)] updates the attributes described by
        the string [attr] of the node [node] to have the value [value].  See
        [Get_node_attr] for attribute descriptions. *)
  | Remove_handlers of 'a expr
    (** [Remove_handlers(node)] removes all handlers from the "textbox" or
        "button" node [node].  It is an error if [node] is not a "textbox" or
        "button" node. *)
  | Add_handler of 'a expr * 'a expr
    (** [Add_handler(node, h)] adds the [Function] value [h] as a handler to the
        "textbox" or "button" node [node].  [h] will be applied to [node] itself
        at the time the handler is triggered. *)

  | Get_parent of 'a expr
    (** [Get_parent(node)] returns the parent node of [node] if it has one or
        returns [null] if it does not (or if it is the root node of a page). *)
  | Get_child of 'a expr * 'a expr
    (** [Get_child(node, pos)] returns the child node of [node] in
        position [pos] (counting from 0), or [null] if [node] has less than
        [pos + 1] children.  It is an error if [node] is not a "div" node. *)
  | Insert_node of 'a expr * 'a expr * 'a expr
    (** [Insert_node(parent, node, pos)] inserts [node] as the child at
        position [pos] of the "div" node [parent].  It is an error if [parent]
        is not a "div" node; it is an error if [node] is an ancestor of
        [parent]; it is an error if [parent] has less than [pos] nodes. *)
  | Remove_node of 'a expr
    (** [Remove_node(node)] removes [node] (and therefore all of its
        descendents) from its parent node or removes directly from its enclosing
        window if it is a document root node. *)

(** {2 HTML documents} *)

(** A type to use for the [id] attribute of an HTML tag. *)
type elt_id = {
  elt_id_value: string;
}

(** A type representing a simplified form of HTML documents. *)
type doc =
  | Para of elt_id option * string
    (** [Para(id, text)] represents a block of text. *)
  | Link of elt_id option * url * string
    (** [Link(id, link_text, href)] represents to an HTML [a] tag. *)
  | Textbox of elt_id option * string
    (** [Textbox(id, contents)] represents a text input box. *)
  | Button of elt_id option * string
    (** [Button(id, button_text)] represents to an HTML button. *)
  | Inl_script of elt_id option * void expr
    (** Represents a [script] tag with an inline script. *)
  | Rem_script of elt_id option * url
    (** Represents a [script] tag with a [src] attribute. *)
  | Div of elt_id option * doc list
    (** Contains a sequence of subdocuments.  For simplicity, this is the only
        document structure with children. *)

(** {2 Network (HTTP) interface} *)

(** The type of an HTTP request with its headers.  We do not distinguish between
    GET and POST requests. *)
type req = {
  req_uri: req_uri;
  req_cookies: (string * string) list;
  req_body: string;
}

(** The type of an HTTP response payload. *)
type file_data =
  | Empty_file
  | Html_file of doc list
  | Script_file of void expr

(** The type of an HTTP response with its headers.  Setting a cookie takes
    priority over deleting the same cookie. *)
type resp = {
  resp_del_cookies: string list;
  resp_set_cookies: (string * string) list;
  resp_body: file_data;
}

(** A type for identifying a unique network connection.  The integer 0
    represents the most recent connection to a given domain, 1 represents the
    next-most-recent, and so on. *)
type net_connection =
  Net_connection of domain * int

(** {2 User interface} *)

(** A type for the visible representation of a document node tree. *)
type rendered_doc =
  | Para_rendered of string
  | Link_rendered of url * string
  | Textbox_rendered of string
  | Button_rendered of string
  | Div_rendered of rendered_doc list

(** A type for identifying a unique browser window.  The integer 0 represents
    the most recently (re)loaded window with a given URL, 1 represents the
    next-most-recent, and so on. *)
type user_window =
  | User_window of url * int

(** A type for identifying a text input box.  The integer refers to the position
    of the text box among all of the text boxes on a given page. *)
type user_textbox =
  | User_textbox of user_window * int

(** A type for identifying a button.  The integer refers to the position of the
    button among all of the buttons on a given page. *)
type user_button =
  | User_button of user_window * int

(** {2 Input/output actions} *)

(** The type of input events that can trigger action in a browser. *)
type input_event =
  | User_load_in_new_win_event of url
    (** Represents the action of a user opening a new window at a the given
        URL. *)
  | User_load_in_win_event of user_window * url
    (** Represents the action of a user loading a new URL in a window.  The may
        be the result of typing in the address bar or of clicking on a link in
        the window's current page. *)
  | User_link_to_new_win_event of user_window * url
    (** Represents the action of a user loading a URL from one page in a new
        window.  This may be the result of a clicking on a link with a [target]
        attribute set to "_blank" or of clicking on a link in a special way
        (e.g., with the control key pressed). *)
  | User_link_to_named_win_event of user_window * string * url
    (** Represents the action of a user loading a URL from one page in a window
        with a particular name.  This may be the result of clicking on a link
        with a [target] attribute set to some particular name. *)
  | User_close_win_event of user_window
    (** Represents the action of a user closing a window. *)
  | User_input_text_event of user_textbox * string * level option
    (** Represents the action of a user updating the text in a text input
        box. *)
  | User_click_button_event of user_button
    (** Represents the action of a user clicking on a button in a page. *)
  | Network_response_event of net_connection * resp
    (** Represents the receipt of an HTTP response. *)

(** The type of output events that a browser can generate. *)
type output_event =
  | UI_win_opened_event (* windows  open to the URL "about:blank" *)
    (** Represents the opening of a new window.  Windows always open with the
        initial location [about:blank], even if an HTTP request has been sent
        for a document that will eventually be loaded in the page. *)
  | UI_win_closed_event of user_window
    (** Represents the closing of a window. *)
  | UI_page_loaded_event of user_window * url * rendered_doc option
    (** Represents a new document being loaded in a window.  If the rendered
        document is [None], then the updated page has no visible contents. *)
  | UI_page_updated_event of user_window * rendered_doc option
    (** Represents an update to the structure of a page.  The entire page is
        included in this event representation, even if the update only affected
        some portiion of the page. *)
  | UI_alert of string
    (** Represents a pop-up box with a message. *)
  | UI_error of string
    (** Represents an error event. *)
  | Network_send_event of domain * req
    (** Represents an HTTP request. *)

