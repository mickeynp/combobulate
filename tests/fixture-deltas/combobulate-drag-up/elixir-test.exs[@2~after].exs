# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 203) (2 outline 235) (3 outline 280)); -*-
defmodule DemoTest do
  use ExUnit.Case

  describe "run/1" do
    test "one" do
      assert true
    end

    setup do
      :ok
    end

    test "two" do
      assert true
    end
  end
end
