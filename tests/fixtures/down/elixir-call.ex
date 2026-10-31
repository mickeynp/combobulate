# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 177) (2 outline 186) (3 outline 187)); -*-
defmodule Demo do
  def run(list) do
    Enum.map([1, 2], fn x -> x end)
  end
end
