# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 174) (2 outline 188) (3 outline 202)); -*-
defmodule Demo do
  def run(x) do
    b = a * 2
    a = x + 1
    log(b)
  end
end
