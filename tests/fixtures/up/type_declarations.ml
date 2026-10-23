(* -*- combobulate-test-point-overlays: ((1 outline 188) (2 outline 193) (3 outline 215) (4 outline 217) (5 outline 228) (6 outline 236)); eval: (combobulate-test-fixture-mode t); -*- *)
type second_level_page =
  | Regular of {
      header : unwind_info_section_header;
      entries : unwind_info_regular_second_level_entry array;
    }
  | Compressed of {
      header : unwind_info_compressed_section_header;
      encoding_array : compact_unwind_encoding array;
      entries : unwind_info_compressed_second_level_entry array;
    }
