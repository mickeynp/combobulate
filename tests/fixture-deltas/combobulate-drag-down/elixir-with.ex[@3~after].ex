# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 179) (2 outline 210) (3 outline 241)); -*-
defmodule Demo do
  def run(x) do
    with {:ok, a} <- fetch(x),
         {:ok, b} <- check(a),
         {:ok, c} <- save(b) do
      c
    end
  end
end
