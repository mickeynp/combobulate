(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 172) (2 outline 177) (3 outline 201) (4 outline 212) (5 outline 220)); -*- *)
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
