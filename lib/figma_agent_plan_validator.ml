open Figma_types

type task_ref = {
  id: string;
  node_id: string;
  depends_on: string list;
}

let known_top_fields = [ "root_node_id"; "tasks"; "planner"; "metadata" ]
let known_task_fields =
  [ "id"; "node_id"; "title"; "description"; "depends_on"; "kind"; "metadata" ]

let get_string_field key fields =
  match List.assoc_opt key fields with
  | Some (`String value) when String.trim value <> "" -> Some (String.trim value)
  | _ -> None

let get_string_list_field key fields =
  match List.assoc_opt key fields with
  | Some (`List values) ->
      values
      |> List.filter_map (function
           | `String value when String.trim value <> "" -> Some (String.trim value)
           | _ -> None)
      |> Option.some
  | None -> Some []
  | _ -> None

let append_unique target value =
  if List.mem value !target then () else target := value :: !target

let validate_json ~root_node_id (root : ui_node) (plan : Yojson.Safe.t) =
  let errors = ref [] in
  let warnings = ref [] in
  let append_error = append_unique errors in
  let append_warning = append_unique warnings in
  let flattened = Figma_planning_context.flatten_tree root in
  let node_index = Hashtbl.create 64 in
  List.iter
    (fun (flat : Figma_planning_context.flat_node) ->
      Hashtbl.replace node_index (Figma_api.normalize_node_id flat.node.id) flat)
    flattened;
  let parsed_tasks = ref [] in
  (match plan with
   | `Assoc top_fields ->
       List.iter
         (fun (key, _) ->
           if not (List.mem key known_top_fields) then
             append_warning ("unknown_top_field:" ^ key))
         top_fields;
       (match get_string_field "root_node_id" top_fields with
        | Some provided_root ->
            let normalized = Figma_api.normalize_node_id provided_root in
            if normalized <> Figma_api.normalize_node_id root_node_id then
              append_error
                (Printf.sprintf "root_node_id_mismatch:%s" provided_root)
        | None -> ());
       (match List.assoc_opt "tasks" top_fields with
        | Some (`List tasks) ->
            if tasks = [] then append_error "empty_tasks";
            List.iteri
              (fun index task_json ->
                match task_json with
                | `Assoc task_fields ->
                    List.iter
                      (fun (key, _) ->
                        if not (List.mem key known_task_fields) then
                          append_warning
                            (Printf.sprintf "task[%d].unknown_field:%s" index key))
                      task_fields;
                    let id = get_string_field "id" task_fields in
                    let node_id = get_string_field "node_id" task_fields in
                    let depends_on = get_string_list_field "depends_on" task_fields in
                    (match (id, node_id, depends_on) with
                     | Some id, Some node_id, Some depends_on ->
                         parsed_tasks :=
                           {
                             id;
                             node_id = Figma_api.normalize_node_id node_id;
                             depends_on;
                           }
                           :: !parsed_tasks
                     | _ ->
                         append_error
                           (Printf.sprintf "task[%d].invalid_shape" index))
                | _ ->
                    append_error (Printf.sprintf "task[%d].not_object" index))
              tasks
        | Some _ -> append_error "tasks_not_list"
        | None -> append_error "missing_tasks")
   | _ -> append_error "plan_not_object");
  let parsed_tasks = List.rev !parsed_tasks in
  let task_ids = Hashtbl.create 32 in
  let node_assignments = Hashtbl.create 32 in
  List.iter
    (fun task ->
      let current = Option.value ~default:0 (Hashtbl.find_opt task_ids task.id) in
      Hashtbl.replace task_ids task.id (current + 1);
      let assigned =
        Option.value ~default:[] (Hashtbl.find_opt node_assignments task.node_id)
      in
      Hashtbl.replace node_assignments task.node_id (task.id :: assigned);
      if not (Hashtbl.mem node_index task.node_id) then
        append_error
          (Printf.sprintf "task[%s].node_out_of_root:%s" task.id task.node_id);
      List.iter
        (fun dep_id ->
          if dep_id = task.id then
            append_error (Printf.sprintf "task[%s].self_dependency" task.id))
        task.depends_on)
    parsed_tasks;
  Hashtbl.iter
    (fun task_id count ->
      if count > 1 then append_error ("duplicate_task_id:" ^ task_id))
    task_ids;
  Hashtbl.iter
    (fun node_id task_ids_for_node ->
      if List.length task_ids_for_node > 1 then
        append_warning
          (Printf.sprintf "multiple_tasks_for_node:%s" node_id))
    node_assignments;
  List.iter
    (fun task ->
      List.iter
        (fun dep_id ->
          if not (Hashtbl.mem task_ids dep_id) then
            append_error
              (Printf.sprintf "task[%s].unknown_dependency:%s" task.id dep_id))
        task.depends_on)
    parsed_tasks;
  let graph =
    parsed_tasks
    |> List.to_seq
    |> Seq.map (fun task -> (task.id, task.depends_on))
    |> Hashtbl.of_seq
  in
  let visiting = Hashtbl.create 32 in
  let visited = Hashtbl.create 32 in
  let rec visit task_id =
    if Hashtbl.mem visiting task_id then
      append_error ("dependency_cycle:" ^ task_id)
    else if not (Hashtbl.mem visited task_id) then begin
      Hashtbl.replace visiting task_id true;
      let deps = Option.value ~default:[] (Hashtbl.find_opt graph task_id) in
      List.iter visit deps;
      Hashtbl.remove visiting task_id;
      Hashtbl.replace visited task_id true
    end
  in
  List.iter (fun task -> visit task.id) parsed_tasks;
  let resolved_nodes =
    parsed_tasks
    |> List.filter_map (fun task ->
         match Hashtbl.find_opt node_index task.node_id with
         | None -> None
         | Some flat ->
             Some
               (`Assoc [
                 ("task_id", `String task.id);
                 ("node_id", `String flat.node.id);
                 ("name", `String flat.node.name);
                 ("type",
                  `String (Figma_query.node_type_to_string flat.node.node_type));
                 ("parent_node_id",
                  match flat.parent_node_id with
                  | Some id -> `String id
                  | None -> `Null);
                 ("depth", `Int flat.depth);
               ]))
  in
  `Assoc [
    ("valid", `Bool (!errors = []));
    ("root_node_id", `String root_node_id);
    ("task_count", `Int (List.length parsed_tasks));
    ("errors", `List (List.rev_map (fun msg -> `String msg) !errors));
    ("warnings", `List (List.rev_map (fun msg -> `String msg) !warnings));
    ("resolved_nodes", `List resolved_nodes);
  ]
