(** The definition of a reactive system. *)

module type REACTIVE_SYSTEM_TYPE = sig
  type waiting
  type running
  type input_event
  type output_event

  type state =
    | Waiting of waiting
    | Running of running

  val start: state

  val receive: input_event -> waiting -> state * output_event list

  val continue: running -> state * output_event list

end

