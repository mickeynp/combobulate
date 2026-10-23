(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 213) (2 outline 268)); -*- *)
let detect_format_and_arch buf =
  match format with
  | ELF -> "elf"
  | MACHO ->
      let header, _commands = Object.Macho.read buf in
      Printf.sprintf "%s" arch_str
