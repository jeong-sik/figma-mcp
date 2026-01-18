(** Figma 디자인 통계 분석 *)

open Figma_types

(** ============== 색상 통계 ============== *)

type color_usage = {
  hex: string;
  count: int;
  node_names: string list;
}

let collect_colors nodes =
  let color_map = Hashtbl.create 32 in

  List.iter (fun node ->
    List.iter (fun paint ->
      match paint.paint_type, paint.color with
      | Solid, Some c ->
          let hex = Printf.sprintf "#%02X%02X%02X"
            (int_of_float (c.r *. 255.))
            (int_of_float (c.g *. 255.))
            (int_of_float (c.b *. 255.))
          in
          let current =
            try Hashtbl.find color_map hex
            with Not_found -> { hex; count = 0; node_names = [] }
          in
          Hashtbl.replace color_map hex {
            current with
            count = current.count + 1;
            node_names = node.name :: current.node_names;
          }
      | _ -> ()
    ) node.fills
  ) nodes;

  Hashtbl.fold (fun _ v acc -> v :: acc) color_map []
  |> List.sort (fun a b -> compare b.count a.count)

(** ============== 폰트 통계 ============== *)

type font_usage = {
  family: string;
  sizes: float list;
  weights: int list;
  count: int;
}

let collect_fonts nodes =
  let font_map = Hashtbl.create 16 in

  List.iter (fun node ->
    match node.typography with
    | Some t ->
        let current =
          try Hashtbl.find font_map t.font_family
          with Not_found -> { family = t.font_family; sizes = []; weights = []; count = 0 }
        in
        Hashtbl.replace font_map t.font_family {
          family = t.font_family;
          sizes = t.font_size :: current.sizes;
          weights = t.font_weight :: current.weights;
          count = current.count + 1;
        }
    | None -> ()
  ) nodes;

  Hashtbl.fold (fun _ v acc ->
    { v with
      sizes = List.sort_uniq compare v.sizes;
      weights = List.sort_uniq compare v.weights;
    } :: acc
  ) font_map []
  |> List.sort (fun a b -> compare b.count a.count)

(** ============== 크기 통계 ============== *)

type size_stats = {
  min_width: float;
  max_width: float;
  avg_width: float;
  min_height: float;
  max_height: float;
  avg_height: float;
  common_widths: (float * int) list;
  common_heights: (float * int) list;
}

let collect_sizes nodes =
  let nodes_with_size = List.filter_map (fun n ->
    match n.bbox with
    | Some b -> Some (b.width, b.height)
    | None -> None
  ) nodes in

  if List.length nodes_with_size = 0 then None
  else begin
    let widths = List.map fst nodes_with_size in
    let heights = List.map snd nodes_with_size in
    let n = float_of_int (List.length nodes_with_size) in

    let count_occurrences values =
      let map = Hashtbl.create 32 in
      List.iter (fun v ->
        let rounded = Float.round v in
        let current = try Hashtbl.find map rounded with Not_found -> 0 in
        Hashtbl.replace map rounded (current + 1)
      ) values;
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) map []
      |> List.sort (fun (_, a) (_, b) -> compare b a)
      |> List.filteri (fun i _ -> i < 5)
    in

    Some {
      min_width = List.fold_left min Float.max_float widths;
      max_width = List.fold_left max Float.min_float widths;
      avg_width = (List.fold_left (+.) 0. widths) /. n;
      min_height = List.fold_left min Float.max_float heights;
      max_height = List.fold_left max Float.min_float heights;
      avg_height = (List.fold_left (+.) 0. heights) /. n;
      common_widths = count_occurrences widths;
      common_heights = count_occurrences heights;
    }
  end

(** ============== 컴포넌트 통계 ============== *)

type component_stats = {
  component_count: int;
  instance_count: int;
  component_set_count: int;
  unused_components: string list;  (* 인스턴스가 없는 컴포넌트 *)
}

let collect_component_stats nodes =
  let components = List.filter (fun n -> n.node_type = Component) nodes in
  let instances = List.filter (fun n -> n.node_type = Instance) nodes in
  let component_sets = List.filter (fun n -> n.node_type = ComponentSet) nodes in

  (* 사용된 컴포넌트 ID 수집 (간단한 휴리스틱) *)
  let used_names = List.map (fun n -> n.name) instances in

  let unused = List.filter_map (fun c ->
    if List.exists (fun name -> String.length name > 0 && name = c.name) used_names then None
    else Some c.name
  ) components in

  {
    component_count = List.length components;
    instance_count = List.length instances;
    component_set_count = List.length component_sets;
    unused_components = List.filteri (fun i _ -> i < 10) unused;  (* 최대 10개 *)
  }

(** ============== 전체 통계 리포트 ============== *)

type full_stats = {
  total_nodes: int;
  nodes_by_type: (string * int) list;
  colors: color_usage list;
  fonts: font_usage list;
  sizes: size_stats option;
  components: component_stats;
}

let generate_report nodes =
  (* 타입별 집계 *)
  let type_map = Hashtbl.create 16 in
  List.iter (fun n ->
    let t = Figma_query.node_type_to_string n.node_type in
    let current = try Hashtbl.find type_map t with Not_found -> 0 in
    Hashtbl.replace type_map t (current + 1)
  ) nodes;
  let nodes_by_type = Hashtbl.fold (fun k v acc -> (k, v) :: acc) type_map []
    |> List.sort (fun (_, a) (_, b) -> compare b a) in

  {
    total_nodes = List.length nodes;
    nodes_by_type;
    colors = collect_colors nodes;
    fonts = collect_fonts nodes;
    sizes = collect_sizes nodes;
    components = collect_component_stats nodes;
  }

let report_to_string stats =
  let lines = Buffer.create 1024 in

  Buffer.add_string lines "═══════════════════════════════════════\n";
  Buffer.add_string lines "          📊 FIGMA 디자인 통계          \n";
  Buffer.add_string lines "═══════════════════════════════════════\n\n";

  (* 노드 개요 *)
  Buffer.add_string lines (Printf.sprintf "📐 총 노드 수: %d\n\n" stats.total_nodes);

  Buffer.add_string lines "📋 타입별 분포:\n";
  List.iter (fun (t, c) ->
    Buffer.add_string lines (Printf.sprintf "   %-15s %4d개\n" t c)
  ) stats.nodes_by_type;

  (* 색상 *)
  Buffer.add_string lines "\n🎨 사용된 색상 (상위 10개):\n";
  List.iteri (fun i c ->
    if i < 10 then
      Buffer.add_string lines (Printf.sprintf "   %s  %3d회\n" c.hex c.count)
  ) stats.colors;

  (* 폰트 *)
  Buffer.add_string lines "\n✏️ 사용된 폰트:\n";
  List.iter (fun (f: font_usage) ->
    let sizes_str = String.concat ", " (List.map (Printf.sprintf "%.0f") f.sizes) in
    Buffer.add_string lines (Printf.sprintf "   %s (%d회) - 크기: %s\n" f.family f.count sizes_str)
  ) stats.fonts;

  (* 크기 *)
  (match stats.sizes with
  | Some s ->
      Buffer.add_string lines "\n📏 크기 통계:\n";
      Buffer.add_string lines (Printf.sprintf "   너비: %.0f ~ %.0f (평균 %.0f)\n" s.min_width s.max_width s.avg_width);
      Buffer.add_string lines (Printf.sprintf "   높이: %.0f ~ %.0f (평균 %.0f)\n" s.min_height s.max_height s.avg_height);
      Buffer.add_string lines "   자주 사용되는 너비: ";
      Buffer.add_string lines (String.concat ", " (List.map (fun (w, c) -> Printf.sprintf "%.0f(%d)" w c) s.common_widths));
      Buffer.add_string lines "\n"
  | None -> ());

  (* 컴포넌트 *)
  let c = stats.components in
  Buffer.add_string lines "\n🧩 컴포넌트 통계:\n";
  Buffer.add_string lines (Printf.sprintf "   컴포넌트: %d개\n" c.component_count);
  Buffer.add_string lines (Printf.sprintf "   컴포넌트셋: %d개\n" c.component_set_count);
  Buffer.add_string lines (Printf.sprintf "   인스턴스: %d개\n" c.instance_count);
  if List.length c.unused_components > 0 then begin
    Buffer.add_string lines "   ⚠️ 미사용 컴포넌트:\n";
    List.iter (fun name ->
      Buffer.add_string lines (Printf.sprintf "      - %s\n" name)
    ) c.unused_components
  end;

  Buffer.add_string lines "\n═══════════════════════════════════════\n";

  Buffer.contents lines
