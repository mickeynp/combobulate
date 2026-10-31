# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 172) (2 outline 191) (3 outline 207) (4 outline 213)); -*-
defmodule Demo do
  def pick(x) do
    case x do
      :a -> 1
      :b -> 2
    end
  end
end
