(* -*- combobulate-test-point-overlays: ((1 outline 192) (2 outline 226) (3 outline 288)); eval: (combobulate-test-fixture-mode t); -*- *)

class virtual file_handler filename =
  object
    val mutable file_descr = None
    method open_file = file_descr <- Some (open_out filename)
    method write_line str =
      match file_descr with
      | Some oc -> output_string oc (str ^ "\n")
      | None -> failwith "File not open"
  end


