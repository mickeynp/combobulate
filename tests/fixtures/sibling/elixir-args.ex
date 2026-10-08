# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 198) (2 outline 202) (3 outline 208)); -*-
defmodule Demo do
  def run(id, opts, timeout) do
    request(id, opts, timeout)
  end
end
