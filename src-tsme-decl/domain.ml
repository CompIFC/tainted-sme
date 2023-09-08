(** Defines a type for internet domain names. *)

(** The type of an fully qualified internet domain name. *)
type domain = {
  domain_value: string list;
  (** A sequence of subdomain identifiers with the TLD written first:
      for example, [\["edu"; "upenn"; "cis"\]]. *)
}

