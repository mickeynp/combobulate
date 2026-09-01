(* -*- combobulate-test-point-overlays: ((1 outline 196) (2 outline 246) (3 outline 276)); eval: (combobulate-test-fixture-mode t); -*- *)
class virtual file_handler : string ->
    object
      val mutable file_descr : out_channel option
      method write_line : string -> unit
      method open_file : unit
    end
