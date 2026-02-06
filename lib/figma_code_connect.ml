(** Code Connect-style component mapping (repo-local, deterministic).

    This is intentionally small: JSON parsing + validation + matching.
    LLM-assisted suggestions (if ever added) must be optional and never required
    for correctness. *)

type figma = {
  node_id : string option;
  component_key : string option;
  name : string;
  variant : (string * string) list;
}

type code = {
  package : string option;
  file : string option;
  export : string;
  props : (string * string) list;
}

type component = {
  id : string;
  figma : figma;
  code : code;
  aliases : string list;
  tags : string list;
}

type project = {
  name : string option;
  repo : string option;
  design_system : string option;
}

type mapping = {
  version : string;
  project : project option;
  components : component list;
}

type diagnostic = {
  path : string;
  message : string;
}

let diag ~path message = { path; message }

let obj_of_json ~errors ~path = function
  | `Assoc kv -> kv
  | `Null -> []
  | _ ->
      errors := diag ~path "expected object" :: !errors;
      []

let list_of_json ~errors ~path = function
  | `List xs -> xs
  | `Null -> []
  | _ ->
      errors := diag ~path "expected array" :: !errors;
      []

let string_opt_of_json ~errors ~path = function
  | `String s -> Some s
  | `Null -> None
  | _ ->
      errors := diag ~path "expected string" :: !errors;
      None

let string_of_json ~errors ~path ~default json =
  string_opt_of_json ~errors ~path json |> Option.value ~default

let string_list_of_json ~errors ~path json =
  let xs = list_of_json ~errors ~path json in
  List.filter_map
    (fun v ->
      match v with
      | `String s -> Some s
      | `Null -> None
      | _ ->
          errors := diag ~path "expected array of strings" :: !errors;
          None)
    xs

let kv_string_list_of_json ~errors ~path json =
  match json with
  | `Null -> []
  | `Assoc kv ->
      List.filter_map
        (fun (k, v) ->
          match v with
          | `String s -> Some (k, s)
          | `Null -> None
          | _ ->
              errors := diag ~path ("expected string values (key: " ^ k ^ ")") :: !errors;
              None)
        kv
  | _ ->
      errors := diag ~path "expected object (string map)" :: !errors;
      []

let obj_get key obj = List.assoc_opt key obj |> Option.value ~default:`Null

let parse_json json =
  let errors = ref [] in
  let root = obj_of_json ~errors ~path:"$" json in

  let version =
    string_of_json ~errors ~path:"$.version" ~default:""
      (obj_get "version" root)
  in

  let project =
    match obj_get "project" root with
    | `Null -> None
    | proj_json ->
        let proj = obj_of_json ~errors ~path:"$.project" proj_json in
        Some
          {
            name = string_opt_of_json ~errors ~path:"$.project.name" (obj_get "name" proj);
            repo = string_opt_of_json ~errors ~path:"$.project.repo" (obj_get "repo" proj);
            design_system =
              string_opt_of_json ~errors ~path:"$.project.design_system"
                (obj_get "design_system" proj);
          }
  in

  let components_json = list_of_json ~errors ~path:"$.components" (obj_get "components" root) in
  let components =
    components_json
    |> List.mapi (fun i comp_json ->
         let base_path = Printf.sprintf "$.components[%d]" i in
         let comp = obj_of_json ~errors ~path:base_path comp_json in

         let figma_json = obj_get "figma" comp in
         let figma_obj = obj_of_json ~errors ~path:(base_path ^ ".figma") figma_json in
         let figma =
           {
             node_id =
               string_opt_of_json ~errors ~path:(base_path ^ ".figma.node_id")
                 (obj_get "node_id" figma_obj);
             component_key =
               string_opt_of_json ~errors ~path:(base_path ^ ".figma.component_key")
                 (obj_get "component_key" figma_obj);
             name =
               string_of_json ~errors ~path:(base_path ^ ".figma.name") ~default:""
                 (obj_get "name" figma_obj);
             variant =
               kv_string_list_of_json ~errors ~path:(base_path ^ ".figma.variant")
                 (obj_get "variant" figma_obj);
           }
         in

         let code_json = obj_get "code" comp in
         let code_obj = obj_of_json ~errors ~path:(base_path ^ ".code") code_json in
         let code =
           {
             package =
               string_opt_of_json ~errors ~path:(base_path ^ ".code.package")
                 (obj_get "package" code_obj);
             file =
               string_opt_of_json ~errors ~path:(base_path ^ ".code.file")
                 (obj_get "file" code_obj);
             export =
               string_of_json ~errors ~path:(base_path ^ ".code.export") ~default:""
                 (obj_get "export" code_obj);
             props =
               kv_string_list_of_json ~errors ~path:(base_path ^ ".code.props")
                 (obj_get "props" code_obj);
           }
         in

         let aliases =
           string_list_of_json ~errors ~path:(base_path ^ ".aliases") (obj_get "aliases" comp)
         in
         let tags =
           string_list_of_json ~errors ~path:(base_path ^ ".tags") (obj_get "tags" comp)
         in
         {
           id =
             string_of_json ~errors ~path:(base_path ^ ".id") ~default:""
               (obj_get "id" comp);
           figma;
           code;
           aliases;
           tags;
         })
  in

  ({ version; project; components }, List.rev !errors)

let mapping_of_json json =
  let mapping, _errors = parse_json json in
  mapping

let validate mapping =
  let errors = ref [] in

  if not (String.equal mapping.version "1.0") then
    errors := diag ~path:"$.version" "invalid version (expected 1.0)" :: !errors;

  let seen = Hashtbl.create 16 in
  List.iteri
    (fun i c ->
      let base_path = Printf.sprintf "$.components[%d]" i in
      if String.trim c.id = "" then
        errors := diag ~path:(base_path ^ ".id") "missing id" :: !errors
      else if Hashtbl.mem seen c.id then
        errors := diag ~path:(base_path ^ ".id") ("duplicate id: " ^ c.id) :: !errors
      else Hashtbl.add seen c.id true;

      if String.trim c.figma.name = "" then
        errors := diag ~path:(base_path ^ ".figma.name") ("missing figma.name: " ^ c.id) :: !errors;

      if String.trim c.code.export = "" then
        errors := diag ~path:(base_path ^ ".code.export") ("missing code.export: " ^ c.id) :: !errors)
    mapping.components;

  List.rev !errors

let norm_name s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' -> Buffer.add_char buf (Char.lowercase_ascii c)
      | 'a' .. 'z' | '0' .. '9' -> Buffer.add_char buf c
      | _ -> ())
    s;
  Buffer.contents buf

let tokens s =
  let s = String.lowercase_ascii s in
  let rec split acc cur i =
    if i >= String.length s then
      let acc = if cur = "" then acc else cur :: acc in
      List.rev acc
    else
      match s.[i] with
      | ' ' | '/' | '-' | '_' ->
          let acc = if cur = "" then acc else cur :: acc in
          split acc "" (i + 1)
      | c -> split acc (cur ^ String.make 1 c) (i + 1)
  in
  split [] "" 0

let jaccard a b =
  let set lst = List.sort_uniq String.compare lst in
  let a = set a in
  let b = set b in
  let rec inter a b acc =
    match (a, b) with
    | x :: xs, y :: ys when x = y -> inter xs ys (x :: acc)
    | x :: xs, y :: ys when x < y -> inter xs ys acc
    | _ :: xs, _ :: ys -> inter xs ys acc
    | _ -> List.rev acc
  in
  let rec union a b acc =
    match (a, b) with
    | [], ys -> List.rev_append acc ys
    | xs, [] -> List.rev_append acc xs
    | x :: xs, y :: ys when x = y -> union xs ys (x :: acc)
    | x :: xs, y :: ys when x < y -> union xs (y :: ys) (x :: acc)
    | x :: xs, y :: ys -> union (x :: xs) ys (y :: acc)
  in
  let inter = inter a b [] in
  let uni = union a b [] in
  if uni = [] then 0.0
  else float_of_int (List.length inter) /. float_of_int (List.length uni)

let variant_bonus ~query ~cand =
  let cand = List.sort_uniq compare cand in
  let query = List.sort_uniq compare query in
  let rec loop qs cs acc =
    match (qs, cs) with
    | (k1, v1) :: qs, (k2, v2) :: cs when k1 = k2 && v1 = v2 -> loop qs cs (acc +. 0.02)
    | (k1, _) :: qs, (k2, _) :: cs when k1 < k2 -> loop qs cs acc
    | _ :: qs, _ :: cs -> loop qs cs acc
    | _ -> acc
  in
  let bonus = loop query cand 0.0 in
  if bonus > 0.10 then 0.10 else bonus

let alias_bonus ~name ~aliases =
  let n = String.lowercase_ascii name in
  if List.exists (fun a -> String.equal (String.lowercase_ascii a) n) aliases then 0.05 else 0.0

let score_match ~query_name ~query_variant ~query_node_id ~query_component_key comp =
  match (query_node_id, comp.figma.node_id) with
  | Some q, Some c when String.equal q c -> (1.0, "node_id exact")
  | _ -> (
      match (query_component_key, comp.figma.component_key) with
      | Some q, Some c when String.equal q c -> (0.95, "component_key exact")
      | _ ->
          let query_name = String.trim query_name in
          if query_name = "" then (0.0, "insufficient query")
          else
            let n_query = norm_name query_name in
            let n_cand = norm_name comp.figma.name in
            if String.equal n_query n_cand then (0.85, "name normalized exact")
            else
              let base = 0.60 +. (jaccard (tokens query_name) (tokens comp.figma.name) *. 0.20) in
              let bonus = variant_bonus ~query:query_variant ~cand:comp.figma.variant in
              let alias = alias_bonus ~name:query_name ~aliases:comp.aliases in
              (min 0.90 (base +. bonus +. alias), "name token overlap"))

let choose ~limit ~query_name ~query_variant ~query_node_id ~query_component_key comps =
  let scored =
    List.map
      (fun c ->
        let score, reason =
          score_match ~query_name ~query_variant ~query_node_id ~query_component_key c
        in
        (score, reason, c))
      comps
  in
  let tie_cmp a b =
    let score_a, _, comp_a = a in
    let score_b, _, comp_b = b in
    let by_score = Float.compare score_b score_a in
    if by_score <> 0 then by_score
    else
      let by_file =
        match (comp_a.code.file, comp_b.code.file) with
        | Some _, None -> -1
        | None, Some _ -> 1
        | _ -> 0
      in
      if by_file <> 0 then by_file else String.compare comp_a.id comp_b.id
  in
  let sorted = List.sort tie_cmp scored in
  let rec take n acc = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> take (n - 1) (x :: acc) xs
  in
  take limit [] sorted

