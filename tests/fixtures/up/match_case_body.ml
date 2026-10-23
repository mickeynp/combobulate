(* -*- combobulate-test-point-overlays: ((1 outline 188) (2 outline 192) (3 outline 223) (4 outline 262) (5 outline 277) (6 outline 281)); eval: (combobulate-test-fixture-mode t); -*- *)
let detect_format_and_arch buf =
  match format with
  | ELF -> "elf"
  | MACHO ->
      let header, _commands = Object.Macho.read buf in
      Printf.sprintf "%s" arch_str
