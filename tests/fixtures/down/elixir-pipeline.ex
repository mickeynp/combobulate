# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 180) (2 outline 193) (3 outline 205)); -*-
defmodule Demo do
  def names(users) do
    users
    |> Enum.filter(& &1.active)
    |> Enum.sort()
  end
end
