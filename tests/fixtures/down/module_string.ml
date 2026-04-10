(* -*- combobulate-test-point-overlays: ((1 outline 232)); eval: (combobulate-test-fixture-mode t); -*- *)
module StringOps = struct
  let uppercase s u = String.uppercase_ascii s
  let lowercase s = String.lowercase_ascii s
  let reverse s =
    let len = String.length s in
    String.init len (fun i -> s.[len - 1 - i])
  let concat_with sep strs = String.concat sep strs
end

(* hierarchy navigation from let. *)
