(* CSS 조정 디버그 테스트 *)
let () =
  let html = "<div style=\"padding: 10px; padding-top: 5px;\">test</div>" in
  Printf.printf "Input:  %s\n" html;

  let adjusted = Visual_verifier.apply_padding_adjustment (2.0, 0.0, 0.0, 0.0) html in
  Printf.printf "Output: %s\n\n" adjusted;

  (* gap 테스트 *)
  let html2 = "<div style=\"gap: 8px;\">test</div>" in
  Printf.printf "Gap Input:  %s\n" html2;
  let adjusted2 = Visual_verifier.apply_gap_adjustment 2.0 html2 in
  Printf.printf "Gap Output: %s\n\n" adjusted2;

  (* width/height 테스트 *)
  let html3 = "<div style=\"width: 100px; height: 50px;\">test</div>" in
  Printf.printf "Size Input:  %s\n" html3;
  let adjusted3 = Visual_verifier.apply_size_adjustment (5.0, -3.0) html3 in
  Printf.printf "Size Output: %s\n" adjusted3
