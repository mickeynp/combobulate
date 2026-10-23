(* -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 257) (2 outline 293) (3 outline 360) (4 outline 425) (5 outline 675) (6 outline 699) (7 outline 732) (8 outline 1174)); -*- *)
let compute old_lines new_lines =
  let n = Array.length old_lines in
  let m = Array.length new_lines in
  (* Build LCS length table *)
  let dp = Array.init (n + 1) (fun _ -> Array.make (m + 1) 0) in
  for i = 1 to n do
    for j = 1 to m do
      if old_lines.(i - 1) = new_lines.(j - 1) then
        dp.(i).(j) <- dp.(i - 1).(j - 1) + 1
      else dp.(i).(j) <- max dp.(i - 1).(j) dp.(i).(j - 1)
    done
  done;
  (* Backtrack to produce edits *)
  let edits = ref [] in
  let i = ref n and j = ref m in
  while !i > 0 || !j > 0 do
    if !i > 0 && !j > 0 && old_lines.(!i - 1) = new_lines.(!j - 1) then begin
      edits := Same old_lines.(!i - 1) :: !edits;
      decr i;
      decr j
    end
    else if !j > 0 && (!i = 0 || dp.(!i).(!j - 1) >= dp.(!i - 1).(!j)) then begin
      edits := Added new_lines.(!j - 1) :: !edits;
      decr j
    end
    else begin
      edits := Removed old_lines.(!i - 1) :: !edits;
      decr i
    end
  done;
  { edits = !edits }
