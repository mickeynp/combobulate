(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 184) (2 outline 214) (3 outline 244) (4 outline 274)); -*- *)
let describe_header buf =
  let magic = read_u32 buf in
  let ncmds = read_u32 buf in
  let flags = read_u32 buf in
  Printf.sprintf "%lx %lu %lx" magic ncmds flags
