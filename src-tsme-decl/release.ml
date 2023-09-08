open Label
open Io

(*
 dval - declassified value
 data - contains a list of declassified values
 *)
module type DChannel =
sig
  val update: value -> unit
  val read: int -> value option (* read takes in a location i and gets the releasevalue *)
end
;;

module DecChannel : DChannel = struct
  let data : (value list) ref = ref []
  let update v = data := (v::(!data))
  let read i = List.nth_opt (!data) i
end
;;

(*
 rstate - internal state of the environment
 dval - declassified value
 *)
module type ReleaseModule =
sig
  val relfunc: input_event -> (value * input_event option)
end
;;


