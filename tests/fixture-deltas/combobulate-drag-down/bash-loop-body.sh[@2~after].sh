# -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 157) (2 outline 169) (3 outline 182)); -*-
for f in *.txt; do
  echo "$f"
  rm "$f"
  wc -l "$f"
done
