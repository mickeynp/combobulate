(* -*- combobulate-test-point-overlays: ((1 outline 228) (2 outline 263) (3 outline 301) (4 outline 357) (5 outline 460) (6 outline 493) (7 outline 526)); eval: (combobulate-test-fixture-mode t); -*- *)
module Math = struct

  type point = { x: int; y: int }

  type vector = { dx: int; dy: int }

  type rect = { top_left: point; bottom_right: point }

  type shape =
    | Point of point
    | Rect of rect
    | Circle of { center: point; radius: int }

  let origin = { x = 0; y = 0 }

  let make_point x y = { x; y }

  let make_vector dx dy = { dx; dy }

end
