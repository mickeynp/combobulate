(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 175) (2 outline 196) (3 outline 239) (4 outline 271)); -*- *)
let render doc =
  let open Format in
  let ppf = formatter_of_buffer doc.buf in
  let () = pp_open_box ppf 0 in
  pp_print_flush ppf ()
