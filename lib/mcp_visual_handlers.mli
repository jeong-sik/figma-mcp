(** Visual verification and comparison handlers.

    Leaf module — handles fidelity loops, image similarity,
    semantic verification, and element comparison. *)

val handle_fidelity_loop : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_image_similarity : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_verify_visual : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_verify_semantic : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_compare_regions : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_evolution_report : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_compare_elements : Yojson.Safe.t -> (Yojson.Safe.t, string) result
val handle_compare : Yojson.Safe.t -> (Yojson.Safe.t, string) result
