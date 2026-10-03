# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 148) (2 outline 165) (3 outline 185)); -*-
greet() {
  local who="$1"
  return 0
  echo "hello $who"
}
