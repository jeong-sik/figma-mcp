(** HTML Metrics Extractor (Playwright via Node.js)

    Used by semantic-first verification (Design IR vs DOM/layout/style).

    This module is split into:
    - extract_json: effectful (spawns node + playwright)
    - parse: pure (Yojson -> typed record)
*)

open Printf
open Figma_types

type text_node = {
  text: string;
  bbox: bbox;
  font_size_px: float option;
  font_weight: int option;
  font_family: string option;
  color: rgba option;
}

type bg_node = {
  bbox: bbox;
  background: rgba option;
  border_radius_px: float option;
}

type t = {
  viewport_width: int;
  viewport_height: int;
  root: bg_node option;
  text_nodes: text_node list;
  bg_nodes: bg_node list;
}

let temp_dir = Figma_config.Visual.temp_dir

let ensure_dir path =
  if Sys.file_exists path then ()
  else
    try Unix.mkdir path 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let counter = Atomic.make 0

let temp_file ~prefix ~ext =
  ensure_dir temp_dir;
  let pid = Unix.getpid () in
  let n = Atomic.fetch_and_add counter 1 in
  let ts_ms = int_of_float (Unix.gettimeofday () *. 1000.0) in
  Filename.concat temp_dir (sprintf "%s_%d_%d_%d.%s" prefix ts_ms pid n ext)

let starts_with ~prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let rgba_of_hex s =
  let s = String.trim s in
  let s = if starts_with ~prefix:"#" s then String.sub s 1 (String.length s - 1) else s in
  let hex_to_int h =
    try int_of_string ("0x" ^ h) with _exn -> 0  (* malformed color value, default to 0 *)
  in
  match String.length s with
  | 6 ->
      let r = hex_to_int (String.sub s 0 2) in
      let g = hex_to_int (String.sub s 2 2) in
      let b = hex_to_int (String.sub s 4 2) in
      Some { r = float r /. 255.0; g = float g /. 255.0; b = float b /. 255.0; a = 1.0 }
  | 8 ->
      let r = hex_to_int (String.sub s 0 2) in
      let g = hex_to_int (String.sub s 2 2) in
      let b = hex_to_int (String.sub s 4 2) in
      let a = hex_to_int (String.sub s 6 2) in
      Some { r = float r /. 255.0; g = float g /. 255.0; b = float b /. 255.0; a = float a /. 255.0 }
  | _ -> None

let rgba_of_css s =
  let s = String.trim s in
  let lower = String.lowercase_ascii s in
  if s = "" || lower = "transparent" || lower = "rgba(0, 0, 0, 0)" then None
  else if starts_with ~prefix:"#" s then rgba_of_hex s
  else if starts_with ~prefix:"rgb(" lower || starts_with ~prefix:"rgba(" lower then
    let inside =
      try
        let l = String.index s '(' in
        let r = String.rindex s ')' in
        String.sub s (l + 1) (r - l - 1)
      with _ -> ""
    in
    let parts =
      inside
      |> String.split_on_char ','
      |> List.map (fun p -> String.trim p)
    in
    let to_int p =
      try int_of_string p with _exn -> 0  (* malformed color value, default to 0 *)
    in
    let to_float p =
      try float_of_string p with _exn -> 0.0  (* unparseable numeric CSS value *)
    in
    match parts with
    | [r; g; b] ->
        Some { r = float (to_int r) /. 255.0; g = float (to_int g) /. 255.0; b = float (to_int b) /. 255.0; a = 1.0 }
    | [r; g; b; a] ->
        Some { r = float (to_int r) /. 255.0; g = float (to_int g) /. 255.0; b = float (to_int b) /. 255.0; a = to_float a }
    | _ -> None
  else None

let float_of_px s =
  let s = String.trim s in
  let len = String.length s in
  let rec take_num i =
    if i >= len then i
    else
      match s.[i] with
      | '0'..'9' | '.' | '-' -> take_num (i + 1)
      | _ -> i
  in
  let n = take_num 0 in
  if n = 0 then None
  else
    let num = String.sub s 0 n in
    try Some (float_of_string num) with _exn -> None  (* unparseable CSS value *)

let int_of_string_opt s =
  let s = String.trim s in
  if s = "" then None else
  try Some (int_of_string s) with _exn -> None  (* unparseable CSS value *)

let metrics_script_path =
  match Sys.getenv_opt "FIGMA_MCP_HTML_METRICS_SCRIPT" with
  | Some path when Sys.file_exists path -> path
  | _ ->
      let candidates = [
        Filename.concat (Sys.getcwd ()) "scripts/extract-html-metrics.js";
        Filename.concat (Filename.dirname Sys.executable_name) "../scripts/extract-html-metrics.js";
      ] in
      List.find_opt Sys.file_exists candidates
      |> Option.value ~default:"scripts/extract-html-metrics.js"

let run_ok ~cmd ~args =
  match Safe_exec.run ~timeout_ms:30000 ~output_limit:(1024 * 1024) cmd args with
  | Error e -> Error e
  | Ok out ->
      if out.timed_out then Error "Process timed out"
      else
        match out.status with
        | Unix.WEXITED 0 -> Ok out
        | Unix.WEXITED code -> Error (sprintf "Exit code %d: %s" code out.stderr)
        | Unix.WSIGNALED sig_num -> Error (sprintf "Killed by signal %d" sig_num)
        | Unix.WSTOPPED sig_num -> Error (sprintf "Stopped by signal %d" sig_num)

let extract_json ?(width=375) ?(height=812) (html : string) : (Yojson.Safe.t, string) result =
  let html_path =
    if Sys.file_exists html then html
    else begin
      let path = temp_file ~prefix:"html_metrics" ~ext:"html" in
      Out_channel.with_open_bin path (fun oc -> output_string oc html);
      path
    end
  in

  let args = [|
    "node";
    metrics_script_path;
    html_path;
    string_of_int width;
    string_of_int height;
  |] in

  match run_ok ~cmd:"node" ~args with
  | Error e -> Error e
  | Ok out ->
      let stdout = String.trim out.stdout in
      try
        let json = Yojson.Safe.from_string stdout in
        let open Yojson.Safe.Util in
        if json |> member "success" |> to_bool then Ok json
        else
          let msg =
            match json |> member "error" with
            | `String s -> s
            | _ -> "Unknown error"
          in
          Error msg
      with e ->
        Error (sprintf "Failed to parse metrics JSON: %s\nOutput: %s" (Printexc.to_string e) stdout)

let parse (json : Yojson.Safe.t) : (t, string) result =
  let open Yojson.Safe.Util in
  try
    let viewport = json |> member "viewport" in
    let viewport_width = viewport |> member "width" |> to_int in
    let viewport_height = viewport |> member "height" |> to_int in

    let parse_bbox b =
      let x = b |> member "x" |> to_float in
      let y = b |> member "y" |> to_float in
      let width = b |> member "width" |> to_float in
      let height = b |> member "height" |> to_float in
      { x; y; width; height }
    in

    let parse_text_node j =
      let text = j |> member "text" |> to_string in
      let bbox = j |> member "bbox" |> parse_bbox in
      let styles = j |> member "styles" in
      let font_size_px =
        match styles |> member "fontSize" |> to_string_option with
        | None -> None
        | Some s -> float_of_px s
      in
      let font_weight =
        match styles |> member "fontWeight" with
        | `String s -> int_of_string_opt s
        | `Int i -> Some i
        | `Float f -> Some (int_of_float f)
        | _ -> None
      in
      let font_family = styles |> member "fontFamily" |> to_string_option in
      let color =
        match styles |> member "color" |> to_string_option with
        | None -> None
        | Some s -> rgba_of_css s
      in
      { text; bbox; font_size_px; font_weight; font_family; color }
    in

    let parse_bg_node j =
      let bbox = j |> member "bbox" |> parse_bbox in
      let styles = j |> member "styles" in
      let background =
        match styles |> member "backgroundColor" |> to_string_option with
        | None -> None
        | Some s -> rgba_of_css s
      in
      let border_radius_px =
        match styles |> member "borderRadius" |> to_string_option with
        | None -> None
        | Some s -> float_of_px s
      in
      { bbox; background; border_radius_px }
    in

    let root =
      match json |> member "root" with
      | `Null -> None
      | j -> Some (parse_bg_node j)
    in

    let text_nodes =
      json |> member "text_nodes" |> to_list |> List.map parse_text_node
    in
    let bg_nodes =
      json |> member "bg_nodes" |> to_list |> List.map parse_bg_node
    in

    Ok { viewport_width; viewport_height; root; text_nodes; bg_nodes }
  with e ->
    Error (sprintf "Invalid html metrics JSON: %s" (Printexc.to_string e))

let extract ?width ?height html : (t, string) result =
  match extract_json ?width ?height html with
  | Error e -> Error e
  | Ok json -> parse json
