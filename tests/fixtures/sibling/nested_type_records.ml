(* -*- combobulate-test-point-overlays: ((1 outline 153) (2 outline 228)); eval: (combobulate-test-fixture-mode t); -*- *)
type second_level_page =
  | Regular of {
      header : string;
      entries : string list;
    }
  | Compressed of {
      header : string;
      encoding_array : string list;
      entries : string list;
    }
