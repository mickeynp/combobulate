# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 152) (2 outline 164) (3 outline 189) (4 outline 194)); -*-
greet() {
  if [ -n "$1" ]; then
    echo "$1"
  fi
}
