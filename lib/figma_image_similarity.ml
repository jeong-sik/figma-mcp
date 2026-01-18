(** Image similarity metrics for rendered Figma comparisons. *)

type image = {
  width: int;
  height: int;
  luma: float array;
}

type metrics = {
  width_a: int;
  height_a: int;
  width_b: int;
  height_b: int;
  overlap_width: int;
  overlap_height: int;
  mse: float;
  psnr: float;
  ssim: float;
}

let find_in_path bin =
  let path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
  let dirs = String.split_on_char ':' path in
  (* Homebrew and common fallback paths for macOS *)
  let fallback_dirs = ["/opt/homebrew/bin"; "/usr/local/bin"; "/usr/bin"] in
  let all_dirs = dirs @ fallback_dirs in
  let rec loop = function
    | [] -> None
    | dir :: rest ->
        let candidate = Filename.concat dir bin in
        if Sys.file_exists candidate then Some candidate
        else loop rest
  in
  loop all_dirs

let run_cmd cmd =
  match Sys.command cmd with
  | 0 -> Ok ()
  | code -> Error (Printf.sprintf "Command failed (%d): %s" code cmd)

let convert_to_ppm ~input ~output =
  let quoted s = Filename.quote s in
  (* PPM 변환: ImageMagick 우선 (sips는 PPM 미지원) *)
  match find_in_path "magick" with
  | Some magick ->
      let cmd = Printf.sprintf "%s %s %s >/dev/null 2>&1"
        magick (quoted input) (quoted output)
      in
      run_cmd cmd
  | None ->
      (match find_in_path "convert" with
       | Some convert ->
           let cmd = Printf.sprintf "%s %s %s >/dev/null 2>&1"
             convert (quoted input) (quoted output)
           in
           run_cmd cmd
       | None ->
           (* sips는 PPM 미지원이므로 BMP로 변환 후 ImageMagick으로 PPM 변환 필요 *)
           (* ImageMagick 없으면 에러 *)
           Error "PPM conversion requires ImageMagick (magick or convert). sips does not support PPM format.")

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let buf = really_input_string ic len in
  close_in ic;
  buf

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let parse_ppm_header data =
  let len = String.length data in
  let idx = ref 0 in
  let skip_ws () =
    while !idx < len && is_space data.[!idx] do
      incr idx
    done
  in
  let rec skip_comment () =
    if !idx < len && data.[!idx] = '#' then begin
      while !idx < len && data.[!idx] <> '\n' do
        incr idx
      done;
      skip_ws ();
      skip_comment ()
    end
  in
  let next_token () =
    skip_ws ();
    skip_comment ();
    skip_ws ();
    let start = !idx in
    while !idx < len && not (is_space data.[!idx]) do
      incr idx
    done;
    if start = !idx then None
    else Some (String.sub data start (!idx - start))
  in
  match (next_token (), next_token (), next_token (), next_token ()) with
  | (Some magic, Some w, Some h, Some maxv) ->
      (magic, int_of_string w, int_of_string h, int_of_string maxv, !idx)
  | _ -> ("", 0, 0, 0, !idx)

let load_ppm path : (image, string) result =
  let data = read_file path in
  let (magic, width, height, maxv, header_end) = parse_ppm_header data in
  if magic <> "P6" then
    Error "Invalid PPM format (expected P6)"
  else if width <= 0 || height <= 0 then
    Error "Invalid PPM dimensions"
  else if maxv <= 0 then
    Error "Invalid PPM maxval"
  else
    let offset = ref header_end in
    while !offset < String.length data && is_space data.[!offset] do
      incr offset
    done;
    let pixel_data = String.sub data !offset (String.length data - !offset) in
    let expected = width * height * 3 in
    if String.length pixel_data < expected then
      Error "PPM pixel data truncated"
    else
      let scale = 1.0 /. float_of_int maxv in
      let luma = Array.make (width * height) 0.0 in
      for i = 0 to (width * height - 1) do
        let base = i * 3 in
        let r = float_of_int (Char.code pixel_data.[base]) in
        let g = float_of_int (Char.code pixel_data.[base + 1]) in
        let b = float_of_int (Char.code pixel_data.[base + 2]) in
        let y = (0.2126 *. r +. 0.7152 *. g +. 0.0722 *. b) *. scale in
        luma.(i) <- y
      done;
      Ok { width; height; luma }

let ensure_ppm path =
  if Filename.extension path = ".ppm" then
    Ok path
  else
    let tmp = Filename.temp_file "figma-compare-" ".ppm" in
    match convert_to_ppm ~input:path ~output:tmp with
    | Ok () -> Ok tmp
    | Error err -> Error err

let mse_psnr a b =
  let width = min a.width b.width in
  let height = min a.height b.height in
  let count = width * height in
  if count = 0 then
    (0.0, infinity, width, height)
  else
    let sum = ref 0.0 in
    for y = 0 to height - 1 do
      let row_a = y * a.width in
      let row_b = y * b.width in
      for x = 0 to width - 1 do
        let da = a.luma.(row_a + x) -. b.luma.(row_b + x) in
        sum := !sum +. (da *. da)
      done
    done;
    let mse = !sum /. float_of_int count in
    let psnr =
      if mse = 0.0 then infinity
      else 10.0 *. Float.log10 (1.0 /. mse)
    in
    (mse, psnr, width, height)

let ssim_window ~a ~b ~x0 ~y0 ~w ~h =
  let n = w * h in
  if n = 0 then 0.0
  else
    let sum_a = ref 0.0 in
    let sum_b = ref 0.0 in
    for y = 0 to h - 1 do
      let row_a = (y0 + y) * a.width in
      let row_b = (y0 + y) * b.width in
      for x = 0 to w - 1 do
        sum_a := !sum_a +. a.luma.(row_a + x0 + x);
        sum_b := !sum_b +. b.luma.(row_b + x0 + x)
      done
    done;
    let mean_a = !sum_a /. float_of_int n in
    let mean_b = !sum_b /. float_of_int n in
    let var_a = ref 0.0 in
    let var_b = ref 0.0 in
    let cov = ref 0.0 in
    for y = 0 to h - 1 do
      let row_a = (y0 + y) * a.width in
      let row_b = (y0 + y) * b.width in
      for x = 0 to w - 1 do
        let da = a.luma.(row_a + x0 + x) -. mean_a in
        let db = b.luma.(row_b + x0 + x) -. mean_b in
        var_a := !var_a +. (da *. da);
        var_b := !var_b +. (db *. db);
        cov := !cov +. (da *. db)
      done
    done;
    let var_a = !var_a /. float_of_int n in
    let var_b = !var_b /. float_of_int n in
    let cov = !cov /. float_of_int n in
    let k1 = 0.01 in
    let k2 = 0.03 in
    let c1 = (k1 *. 1.0) ** 2.0 in
    let c2 = (k2 *. 1.0) ** 2.0 in
    let numerator = (2.0 *. mean_a *. mean_b +. c1) *. (2.0 *. cov +. c2) in
    let denominator = (mean_a *. mean_a +. mean_b *. mean_b +. c1) *. (var_a +. var_b +. c2) in
    if denominator = 0.0 then 0.0 else numerator /. denominator

let ssim a b ~window =
  let width = min a.width b.width in
  let height = min a.height b.height in
  if width = 0 || height = 0 then 0.0
  else
    let win = if window <= 0 then 8 else window in
    let count = ref 0 in
    let sum = ref 0.0 in
    let y = ref 0 in
    while !y + win <= height do
      let x = ref 0 in
      while !x + win <= width do
        let s = ssim_window ~a ~b ~x0:!x ~y0:!y ~w:win ~h:win in
        sum := !sum +. s;
        incr count;
        x := !x + win
      done;
      y := !y + win
    done;
    if !count = 0 then 0.0 else !sum /. float_of_int !count

let compare_paths ~path_a ~path_b : (metrics, string) result =
  match ensure_ppm path_a with
  | Error err -> Error err
  | Ok ppm_a ->
      (match ensure_ppm path_b with
       | Error err -> Error err
       | Ok ppm_b ->
           (match (load_ppm ppm_a, load_ppm ppm_b) with
            | (Ok img_a, Ok img_b) ->
                let (mse, psnr, overlap_w, overlap_h) = mse_psnr img_a img_b in
                let ssim_value = ssim img_a img_b ~window:8 in
                Ok {
                  width_a = img_a.width;
                  height_a = img_a.height;
                  width_b = img_b.width;
                  height_b = img_b.height;
                  overlap_width = overlap_w;
                  overlap_height = overlap_h;
                  mse;
                  psnr;
                  ssim = ssim_value;
                }
            | (Error err, _) -> Error err
            | (_, Error err) -> Error err))

(** Node.js fallback for SSIM comparison when ImageMagick not available *)
let compare_with_nodejs ~path_a ~path_b : (metrics, string) result =
  (* scripts 디렉토리 찾기 *)
  let script_path =
    let candidates = [
      (* worktree 또는 main repo *)
      Filename.concat (Sys.getcwd ()) "scripts/ssim-compare.js";
      Filename.concat (Sys.getcwd ()) "../scripts/ssim-compare.js";
      Filename.concat (Filename.dirname Sys.executable_name) "../scripts/ssim-compare.js";
    ] in
    List.find_opt Sys.file_exists candidates
  in
  match script_path with
  | None -> Error "ssim-compare.js not found"
  | Some script ->
      let cmd = Printf.sprintf "node %s %s %s 2>&1"
        (Filename.quote script)
        (Filename.quote path_a)
        (Filename.quote path_b)
      in
      let ic = Unix.open_process_in cmd in
      (* 파이프에서는 in_channel_length 사용 불가 - 라인 단위로 읽기 *)
      let buf = Buffer.create 1024 in
      (try
        while true do
          Buffer.add_string buf (input_line ic);
          Buffer.add_char buf '\n'
        done
      with End_of_file -> ());
      let _ = Unix.close_process_in ic in
      let output = Buffer.contents buf in
      (* JSON 파싱 *)
      try
        let json = Yojson.Safe.from_string (String.trim output) in
        let open Yojson.Safe.Util in
        match json |> member "error" with
        | `Null ->
            let ssim = json |> member "ssim" |> to_float in
            let psnr = json |> member "psnr" |> to_float in
            let mse = json |> member "mse" |> to_float in
            let width = json |> member "width" |> to_int in
            let height = json |> member "height" |> to_int in
            Ok {
              width_a = width;
              height_a = height;
              width_b = width;
              height_b = height;
              overlap_width = width;
              overlap_height = height;
              mse;
              psnr;
              ssim;
            }
        | error_msg -> Error (to_string error_msg)
      with e -> Error (Printf.sprintf "JSON parse error: %s\nOutput: %s" (Printexc.to_string e) output)

(** 자동 fallback: OCaml native → Node.js *)
let compare_paths_auto ~path_a ~path_b : (metrics, string) result =
  match compare_paths ~path_a ~path_b with
  | Ok metrics -> Ok metrics
  | Error err ->
      (* PPM 변환 실패 또는 다른 에러 시 Node.js fallback 시도 *)
      (match compare_with_nodejs ~path_a ~path_b with
       | Ok m -> Ok m
       | Error _ -> Error err)
